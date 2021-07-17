#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

void __attribute__((noreturn)) die(char *msg)
{
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

void die_with_usage()
{
  die("usage:\n"
      "  riju-system-privileged session UUID LANG [IMAGE-HASH]\n"
      "  riju-system-privileged exec UUID CMDLINE...\n"
      "  riju-system-privileged pty UUID CMDLINE...");
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

void wait_alarm(int signum)
{
  (void)signum;
  die("container did not come up within 10 seconds");
}

void session(char *uuid, char *lang, char *imageHash)
{
  char *image, *container, *hostname, *volume, *fifo;
  if ((imageHash != NULL ? asprintf(&image, "riju:lang-%s-%s", lang, imageHash)
                         : asprintf(&image, "riju:lang-%s", lang)) < 0)
    die("asprintf failed");
  if (asprintf(&container, "riju-session-%s", uuid) < 0)
    die("asprintf failed");
  if (asprintf(&hostname, "HOSTNAME=%s", lang) < 0)
    die("asprintf failed");
  int rv = mkdir("/var/run/riju/sentinels", 0700);
  if (rv < 0 && errno != EEXIST)
    die("mkdir failed");
  char tmpdir[] = "/var/run/riju/sentinels/XXXXXX";
  if (mkdtemp(tmpdir) == NULL)
    die("mkdtemp failed");
  if (asprintf(&volume, "%s:/var/run/riju/sentinel", tmpdir) < 0)
    die("asprintf failed");
  if (asprintf(&fifo, "%s/fifo", tmpdir) < 0)
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
        "3g",
        "--pids-limit",
        "512",
        image,
        "bash",
        "-c",
        "cat /var/run/riju/sentinel/fifo | ( sleep 10; while read -t2; do :; "
        "done; pkill -g0 )",
        NULL,
    };
    execvp(argv[0], argv);
    die("execvp failed");
  }
  struct timespec ts_10ms; // 10ms
  ts_10ms.tv_sec = 0;
  ts_10ms.tv_nsec = 1000 * 1000 * 10;
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
  if (unlink(fifo) < 0)
    die("unlink failed");
  if (rmdir(tmpdir) < 0)
    die("rmdir failed");
  pid = fork();
  if (pid < 0)
    die("fork failed");
  else if (pid == 0) {
    struct timespec ts_1s; // 10ms
    ts_1s.tv_sec = 1;
    ts_1s.tv_nsec = 0;
    while (1) {
      static const char ok[] = "ok\n";
      if (write(fd, ok, sizeof(ok) / sizeof(char)) < 0)
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
  char *container;
  if (asprintf(&container, "riju-session-%s", uuid) < 0)
    die("asprintf failed");
  char *argvPrefix[] = {
      "./system/res/docker-exec.py",
      "--user",
      "riju",
      pty ? "-it" : "-i",
      container,
      "--",
  };
  char **argv = malloc(sizeof(argvPrefix) + (argc + 1) * sizeof(char *));
  if (argv == NULL)
    die("malloc failed");
  memcpy(argv, argvPrefix, sizeof(argvPrefix));
  memcpy((void *)argv + sizeof(argvPrefix), cmdline, argc * sizeof(char *));
  argv[sizeof(argvPrefix) + argc * sizeof(char *)] = NULL;
  execvp(argv[0], argv);
  die("execvp failed");
}

int main(int argc, char **argv)
{
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
    exec(parseUUID(argv[2]), argc, &argv[3], false);
    return 0;
  }
  if (!strcmp(argv[1], "pty")) {
    if (argc < 4)
      die_with_usage();
    exec(parseUUID(argv[2]), argc, &argv[3], true);
    return 0;
  }
  die_with_usage();
}
