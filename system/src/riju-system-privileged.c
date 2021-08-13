#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/random.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#include "sentinel.h"

void __attribute__((noreturn)) die(char *msg)
{
  fprintf(stderr, "%s (errno %d)\n", msg, errno);
  exit(1);
}

void init() { sentinel_bash[sentinel_bash_len - 1] = '\0'; }

void die_with_usage()
{
  die("usage:\n"
      "  riju-system-privileged session UUID LANG [IMAGE-HASH]\n"
      "  riju-system-privileged exec UUID CMDLINE...\n"
      "  riju-system-privileged pty UUID CMDLINE...");
}

char *quoteArgs(int argc, char **cmdline)
{
  char **printfArgs = malloc(sizeof(char *) * (argc + 3));
  printfArgs[0] = "printf";
  printfArgs[1] = "%q ";
  memcpy(printfArgs + 2, cmdline, sizeof(char *) * argc);
  printfArgs[argc + 2] = NULL;
  int fd[2];
  if (pipe(fd) < 0)
    die("pipe failed");
  pid_t pid = fork();
  if (pid < 0)
    die("fork failed");
  else if (pid == 0) {
    if (dup2(fd[1], STDOUT_FILENO) < 0)
      die("dup2 failed");
    if (close(fd[0]) < 0 || close(fd[1]) < 0)
      die("close failed");
    execvp(printfArgs[0], printfArgs);
    die("execvp failed");
  }
  if (close(fd[1]) < 0)
    die("close failed");
  char *buf = malloc(1024);
  if (buf == NULL)
    die("malloc failed");
  ssize_t len_allocated = 1024;
  ssize_t len_total = 0;
  ssize_t len_read;
  while ((len_read = read(fd[0], buf + len_total, 1024)) > 0) {
    len_total += len_read;
    if (len_allocated - len_total < 1024) {
      char *new_buf = malloc(len_allocated + 1024);
      len_allocated += 1024;
      if (new_buf == NULL)
        die("malloc failed");
      memcpy(new_buf, buf, len_total);
      free(buf);
      buf = new_buf;
    }
  }
  if (len_read < 0)
    die("read failed");
  buf[len_total] = '\0';
  return buf;
}

char *getUUID()
{
  char *buf = malloc(16);
  if (buf == NULL)
    die("malloc failed");
  if (getrandom(buf, 16, 0) != 16)
    die("getrandom failed");
  char *uuid;
  if (asprintf(&uuid,
               "%02hhx%02hhx%02hhx%02hhx%02hhx%02hhx%02hhx%02hhx%02hhx%02hhx%"
               "02hhx%02hhx%02hhx%02hhx%02hhx%02hhx",
               buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7],
               buf[8], buf[9], buf[10], buf[11], buf[12], buf[13], buf[14],
               buf[15]) < 0)
    die("asprintf failed");
  return uuid;
}

char *parseUUID(char *uuid)
{
  if (strnlen(uuid, 33) != 32)
    die("illegal uuid");
  for (char *ptr = uuid; *ptr; ++ptr)
    if (!((*ptr >= 'a' && *ptr <= 'z') || (*ptr >= '0' && *ptr <= '9')))
      die("illegal uuid");
  return uuid;
}

char *parseLang(char *lang)
{
  size_t len = strnlen(lang, 65);
  if (len == 0 || len > 64)
    die("illegal language name");
  return lang;
}

char *parseImageHash(char *imageHash)
{
  if (strnlen(imageHash, 41) != 40)
    die("illegal imageHash");
  for (char *ptr = imageHash; *ptr; ++ptr)
    if (!((*ptr >= 'a' && *ptr <= 'z') || (*ptr >= '0' && *ptr <= '9')))
      die("illegal imageHash");
  return imageHash;
}

char *timeout_msg;

void wait_alarm(int signum)
{
  (void)signum;
  die(timeout_msg);
}

void session(char *uuid, char *lang, char *imageHash)
{
  if (setvbuf(stdout, NULL, _IONBF, 0) != 0)
    die("setvbuf failed");
  char *image, *container, *hostname, *share, *volume, *fifo, *rijuPtyPath;
  if ((imageHash != NULL ? asprintf(&image, "riju:lang-%s-%s", lang, imageHash)
                         : asprintf(&image, "riju:lang-%s", lang)) < 0)
    die("asprintf failed");
  if (asprintf(&container, "riju-session-%s", uuid) < 0)
    die("asprintf failed");
  if (asprintf(&hostname, "HOSTNAME=%s", lang) < 0)
    die("asprintf failed");
  if (asprintf(&share, "/var/cache/riju/shares/%s", uuid) < 0)
    die("asprintf failed");
  int rv = mkdir("/var/cache/riju/shares", 0700);
  if (rv < 0 && errno != EEXIST)
    die("mkdir failed");
  rv = mkdir(share, 0700);
  if (rv < 0)
    die("mkdir failed");
  if (asprintf(&rijuPtyPath, "%s/riju-pty", share) < 0)
    die("asprintf failed");
  int fdFrom = open("/src/system/out/riju-pty", O_RDONLY);
  if (fdFrom < 0)
    die("open failed");
  int fdTo = open(rijuPtyPath, O_WRONLY | O_CREAT | O_EXCL, 0700);
  if (fdTo < 0)
    die("open failed");
  char buf[1024];
  int len, len_written;
  while ((len = read(fdFrom, buf, 1024)) > 0) {
    char *ptr = buf;
    while (len > 0) {
      len_written = write(fdTo, ptr, len);
      if (len_written < 0)
        die("write failed");
      len -= len_written;
      ptr += len_written;
    }
  }
  if (close(fdFrom) < 0)
    die("close failed");
  if (close(fdTo) < 0)
    die("close failed");
  if (len < 0)
    die("read failed");
  if (asprintf(&volume, "%s:/var/cache/riju/share", share) < 0)
    die("asprintf failed");
  if (asprintf(&fifo, "%s/control", share) < 0)
    die("asprintf failed");
  if (mknod(fifo, 0700 | S_IFIFO, 0) < 0)
    die("mknod failed");
  pid_t pid = fork();
  if (pid < 0)
    die("fork failed");
  else if (pid == 0) {
    char *argv[] = {
        "docker",
        "run",
        "--rm",
        "-v",
        volume,
        "-e",
        "HOME=/home/riju",
        "-e",
        hostname,
        "-e",
        "LANG=C.UTF-8",
        "-e",
        "LC_ALL=C.UTF-8",
        "-e",
        "LOGNAME=riju",
        "-e",
        "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/bin",
        "-e",
        "PWD=/home/riju/src",
        "-e",
        "SHELL=/usr/bin/bash",
        "-e",
        "TERM=xterm-256color",
        "-e",
        "TMPDIR=/tmp",
        "-e",
        "USER=riju",
        "-e",
        "USERNAME=riju",
        "--user",
        "root",
        "--hostname",
        lang,
        "--name",
        container,
        "--cpus",
        "1",
        "--memory",
        "1g",
        "--memory-swap",
        "8g",
        "--pids-limit",
        "2048",
        "--cgroup-parent",
        "riju.slice",
        image,
        "bash",
        "-c",
        (char *)sentinel_bash,
        NULL,
    };
    execvp(argv[0], argv);
    die("execvp failed");
  }
  struct timespec ts_10ms;
  ts_10ms.tv_sec = 0;
  ts_10ms.tv_nsec = 1000 * 1000 * 10;
  timeout_msg = "container did not come up within 10 seconds";
  signal(SIGALRM, wait_alarm);
  alarm(10);
  int fd;
  while (1) {
    fd = open(fifo, O_WRONLY);
    if (fd >= 0)
      break;
    if (errno != ENXIO)
      die("open failed");
    int rv = nanosleep(&ts_10ms, NULL);
    if (rv != 0 && errno != EINTR)
      die("nanosleep failed");
  }
  signal(SIGALRM, SIG_IGN);
  pid = fork();
  if (pid < 0)
    die("fork failed");
  else if (pid == 0) {
    struct timespec ts_1s;
    ts_1s.tv_sec = 1;
    ts_1s.tv_nsec = 0;
    while (1) {
      static const char ok[] = "ping\n";
      if (write(fd, ok, sizeof(ok) / sizeof(char)) != sizeof(ok) / sizeof(char))
        die("write failed");
      int rv = nanosleep(&ts_1s, NULL);
      if (rv != 0 && errno != EINTR)
        die("nanosleep failed");
    }
  }
  printf("riju: container ready\n"); // magic string
  if (waitpid(pid, NULL, 0) <= 0)
    die("waitpid failed");
  if (close(fd) < 0)
    die("close failed");
}

void exec(char *uuid, int argc, char **cmdline, bool pty)
{
  if (setvbuf(stdout, NULL, _IONBF, 0) != 0)
    die("setvbuf failed");
  char *share, *ctlFIFO, *inputFIFO, *outputFIFO, *ctlCmd, *dataFIFO;
  if (asprintf(&share, "/var/cache/riju/shares/%s", uuid) < 0)
    die("asprintf failed");
  if (asprintf(&ctlFIFO, "%s/control", share) < 0)
    die("asprintf failed");
  char *procUUID = getUUID();
  if (asprintf(&inputFIFO, "%s/cmd-%s-input", share, procUUID) < 0)
    die("asprintf failed");
  if (asprintf(&outputFIFO, "%s/cmd-%s-output", share, procUUID) < 0)
    die("asprintf failed");
  int fd = open(ctlFIFO, O_WRONLY);
  if (fd < 0)
    die("open failed");
  char *quotedArgs = quoteArgs(argc, cmdline);
  int len = asprintf(&ctlCmd, "%s %s %s\n", pty ? "pty" : "exec", procUUID,
                     quotedArgs);
  if (len < 0)
    die("asprintf failed");
  int len_written;
  while ((len_written = write(fd, ctlCmd, len)) > 0) {
    ctlCmd += len_written;
    len -= len_written;
  }
  if (len_written < 0)
    die("write failed");
  close(fd);
  struct timespec ts_10ms;
  ts_10ms.tv_sec = 0;
  ts_10ms.tv_nsec = 1000 * 1000 * 10;
  int mode;
  pid_t pid = fork();
  if (pid < 0)
    die("fork failed");
  else if (pid == 0) {
    dataFIFO = inputFIFO;
    timeout_msg = "sentinel did not set up input FIFO within 1 second";
    mode = O_WRONLY;
  } else {
    dataFIFO = outputFIFO;
    timeout_msg = "sentinel did not set up output FIFO within 1 second";
    mode = O_RDONLY;
  }
  signal(SIGALRM, wait_alarm);
  alarm(1);
  while (1) {
    fd = open(dataFIFO, mode);
    if (fd >= 0)
      break;
    if (errno != ENOENT)
      die("open failed");
    int rv = nanosleep(&ts_10ms, NULL);
    if (rv != 0 && errno != EINTR)
      die("nanosleep failed");
  }
  signal(SIGALRM, SIG_IGN);
  char buf[1024];
  if (pid == 0) {
    while ((len = read(STDIN_FILENO, buf, 1024)) > 0) {
      char *ptr = buf;
      while (len > 0) {
        len_written = write(fd, ptr, len);
        if (len_written < 0)
          die("write failed");
        len -= len_written;
        ptr += len_written;
      }
    }
  } else {
    while ((len = read(fd, buf, 1024)) > 0) {
      fwrite(buf, 1, len, stdout);
      if (ferror(stdout))
        die("fwrite failed");
      if (feof(stdout))
        break;
    }
  }
  if (len < 0)
    die("read failed");
}

int main(int argc, char **argv)
{
  init();
  if (seteuid(0) != 0)
    die("seteuid failed");
  if (argc < 2)
    die_with_usage();
  if (!strcmp(argv[1], "session")) {
    if (argc < 4 || argc > 5)
      die_with_usage();
    char *uuid = parseUUID(argv[2]);
    char *lang = parseLang(argv[3]);
    char *imageHash = argc == 5 ? parseImageHash(argv[4]) : NULL;
    session(uuid, lang, imageHash);
    return 0;
  }
  if (!strcmp(argv[1], "exec")) {
    if (argc < 4)
      die_with_usage();
    exec(parseUUID(argv[2]), argc - 3, &argv[3], false);
    return 0;
  }
  if (!strcmp(argv[1], "pty")) {
    if (argc < 4)
      die_with_usage();
    exec(parseUUID(argv[2]), argc - 3, &argv[3], true);
    return 0;
  }
  die_with_usage();
}
