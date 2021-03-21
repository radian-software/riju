#define _GNU_SOURCE
#include <errno.h>
#include <grp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

void __attribute__ ((noreturn)) die(char *msg)
{
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

void die_with_usage()
{
  die("usage:\n"
      "  riju-system-privileged session UUID LANG\n"
      "  riju-system-privileged wait UUID\n"
      "  riju-system-privileged exec UUID CMDLINE...");
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

char *parseLang(char *lang) {
  size_t len = strnlen(lang, 65);
  if (len == 0 || len > 64)
    die("illegal language name");
  return lang;
}

void session(char *uuid, char *lang)
{
  char *image, *container;
  if (asprintf(&image, "riju:lang-%s", lang) < 0)
    die("asprintf failed");
  if (asprintf(&container, "riju-session-%s", uuid) < 0)
    die("asprintf failed");
  char *argv[] = {
    "docker",
    "run",
    "--rm", "-i",
    "-e", "HOME=/home/riju",
    "-e", "HOSTNAME=riju",
    "-e", "LANG=C.UTF-8",
    "-e", "LC_ALL=C.UTF-8",
    "-e", "LOGNAME=riju",
    "-e", "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/bin",
    "-e", "PWD=/home/riju/src",
    "-e", "SHELL=/usr/bin/bash",
    "-e", "TERM=xterm-256color",
    "-e", "TMPDIR=/tmp",
    "-e", "USER=riju",
    "-e", "USERNAME=riju",
    "--hostname", "riju",
    "--name", container,
    image, "cat", NULL,
  };
  execvp(argv[0], argv);
  die("execvp failed");
}

void wait_alarm(int signum)
{
  (void)signum;
  die("container did not come up within 1 second");
}

void wait(char *uuid)
{
  char *cmdline;
  if (asprintf(&cmdline, "docker inspect riju-session-%s >/dev/null 2>&1", uuid) < 0)
    die("asprintf failed");
  struct timespec ts;
  ts.tv_sec = 0;
  ts.tv_nsec = 1000 * 1000 * 10;
  signal(SIGALRM, wait_alarm);
  alarm(1);
  while (1) {
    FILE *proc = popen(cmdline, "r");
    if (proc == NULL)
      die("popen failed");
    char buf[1024];
    while (fgets(buf, 1024, proc) != NULL);
    if (ferror(proc))
      die("fgets failed");
    int status = pclose(proc);
    if (status < 0)
      die("pclose failed");
    if (WEXITSTATUS(status) == 0)
      break;
    int rv = nanosleep(&ts, NULL);
    if (rv != 0 && rv != EINTR)
      die("nanosleep failed");
  }
}

void exec(char *uuid, int argc, char **cmdline)
{
  char *container;
  if (asprintf(&container, "riju-session-%s", uuid) < 0)
    die("asprintf failed");
  char *argvPrefix[] = {
    "docker",
    "exec",
    "-it",
    container,
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
  if (setuid(0) != 0)
    die("setuid failed");
  if (argc < 2)
    die_with_usage();
  if (!strcmp(argv[1], "session")) {
    if (argc != 4)
      die_with_usage();
    char *uuid = parseUUID(argv[2]);
    char *lang = parseLang(argv[3]);
    session(uuid, lang);
    return 0;
  }
  if (!strcmp(argv[1], "wait")) {
    if (argc != 3)
      die_with_usage();
    char *uuid = parseUUID(argv[2]);
    wait(uuid);
    return 0;
  }
  if (!strcmp(argv[1], "exec")) {
    if (argc < 4)
      die_with_usage();
    exec(parseUUID(argv[2]), argc, &argv[3]);
    return 0;
  }
  die_with_usage();
}
