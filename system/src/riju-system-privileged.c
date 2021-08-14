#define _GNU_SOURCE
#include <ctype.h>
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
  int orig_len = 0;
  for (int i = 0; i < argc; ++i)
    orig_len += strlen(cmdline[i]);
  int quoted_len = orig_len * 2 + argc;
  char *quoted = malloc(sizeof(char) * quoted_len);
  char *quoted_ptr = quoted;
  for (int i = 0; i < argc; ++i) {
    for (char *ptr = cmdline[i]; *ptr != '\0'; ++ptr) {
      if (*ptr == ' ') {
        *(quoted_ptr++) = '+';
        *(quoted_ptr++) = 's';
      } else if (*ptr == '\n') {
        *(quoted_ptr++) = '+';
        *(quoted_ptr++) = 'n';
      } else if (*ptr == '\t') {
        *(quoted_ptr++) = '+';
        *(quoted_ptr++) = 't';
      } else if (*ptr == '+') {
        *(quoted_ptr++) = '+';
        *(quoted_ptr++) = 'p';
      } else if (isprint(*ptr)) {
        *(quoted_ptr++) = *ptr;
      } else {
        die("riju-system-privileged got non-printable char");
      }
    }
    if (i < argc - 1)
      *(quoted_ptr++) = ' ';
  }
  *(quoted_ptr++) = '\0';
  return quoted;
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
  rv = mkdir(share, 0755);
  if (rv < 0)
    die("mkdir failed");
  if (asprintf(&rijuPtyPath, "%s/riju-pty", share) < 0)
    die("asprintf failed");
  int fdFrom = open("/src/system/out/riju-pty", O_RDONLY);
  if (fdFrom < 0)
    die("open failed");
  int fdTo = open(rijuPtyPath, O_WRONLY | O_CREAT | O_EXCL, 0755);
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
  if (mknod(fifo, 0600 | S_IFIFO, 0) < 0)
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
  if (signal(SIGALRM, wait_alarm) == SIG_ERR)
    die("signal failed");
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
  if (signal(SIGALRM, SIG_IGN) == SIG_ERR)
    die("signal failed");
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
  char *share, *ctlFIFO, *inputFIFO, *outputFIFO, *statusFIFO, *ctlCmd,
      *dataFIFO;
  if (asprintf(&share, "/var/cache/riju/shares/%s", uuid) < 0)
    die("asprintf failed");
  if (asprintf(&ctlFIFO, "%s/control", share) < 0)
    die("asprintf failed");
  char *procUUID = getUUID();
  if (asprintf(&inputFIFO, "%s/cmd-%s-input", share, procUUID) < 0)
    die("asprintf failed");
  if (asprintf(&outputFIFO, "%s/cmd-%s-output", share, procUUID) < 0)
    die("asprintf failed");
  if (asprintf(&statusFIFO, "%s/cmd-%s-status", share, procUUID) < 0)
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
  timeout_msg = "sentinel did not set up FIFOs within 1 second";
  struct timespec ts_10ms;
  ts_10ms.tv_sec = 0;
  ts_10ms.tv_nsec = 1000 * 1000 * 10;
  pid_t input_pid = fork(), output_pid;
  if (input_pid < 0)
    die("fork failed");
  else if (input_pid == 0) {
    dataFIFO = inputFIFO;
  } else {
    output_pid = fork();
    if (output_pid < 0)
      die("fork failed");
    else if (output_pid == 0) {
      dataFIFO = outputFIFO;
    } else {
      dataFIFO = statusFIFO;
    }
  }
  if (dataFIFO != statusFIFO) {
    if (signal(SIGALRM, wait_alarm) == SIG_ERR)
      die("signal failed");
    alarm(1);
  }
  while (1) {
    int mode = dataFIFO == inputFIFO ? O_WRONLY : O_RDONLY;
    fd = open(dataFIFO, mode);
    if (fd >= 0)
      break;
    if (errno != ENOENT)
      die("open failed");
    int rv = nanosleep(&ts_10ms, NULL);
    if (rv != 0 && errno != EINTR)
      die("nanosleep failed");
  }
  if (signal(SIGALRM, SIG_IGN) == SIG_ERR)
    die("signal failed");
  char buf[1024];
  if (dataFIFO == inputFIFO) {
    if (close(STDOUT_FILENO) < 0)
      die("close failed");
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
    if (len < 0)
      die("read failed");
  } else if (dataFIFO == outputFIFO) {
    if (close(STDIN_FILENO) < 0)
      die("close failed");
    while ((len = read(fd, buf, 1024)) > 0) {
      fwrite(buf, 1, len, stdout);
      if (ferror(stdout))
        die("fwrite failed");
      if (feof(stdout))
        break;
    }
    if (len < 0)
      die("read failed");
  } else {
    if (close(STDIN_FILENO) < 0)
      die("close failed");
    if (close(STDOUT_FILENO) < 0)
      die("close failed");
    char line[1024];
    char *ptr = line;
    int len;
    while ((len = read(fd, ptr, 1023 - (ptr - line))) > 0) {
      ptr += len;
    }
    if (len < 0)
      die("read failed");
    *ptr = '\0';
    char *endptr;
    long status = strtol(line, &endptr, 10);
    if (*endptr != '\n')
      die("strtol failed");
    if (kill(input_pid, SIGTERM) < 0)
      die("kill failed");
    if (kill(output_pid, SIGTERM) < 0)
      die("kill failed");
    exit(status);
  }
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
