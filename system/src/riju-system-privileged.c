#define _GNU_SOURCE
#include <errno.h>
#include <grp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

// Keep in sync with backend/src/users.ts
const int MIN_UID = 2000;
const int MAX_UID = 65000;

int privileged;

void __attribute__ ((noreturn)) die(char *msg)
{
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

void die_with_usage()
{
  die("usage:\n"
      "  riju-system-privileged useradd UID\n"
      "  riju-system-privileged setup UID UUID\n"
      "  riju-system-privileged spawn UID UUID CMDLINE...\n"
      "  riju-system-privileged teardown UUID");
}

int parseUID(char *str)
{
  if (!privileged)
    return -1;
  char *endptr;
  long uid = strtol(str, &endptr, 10);
  if (!*str || *endptr)
    die("uid must be an integer");
  if (uid < MIN_UID || uid >= MAX_UID)
    die("uid is out of range");
  return uid;
}

char *parseUUID(char *uuid)
{
  if (!*uuid)
    die("illegal uuid");
  for (char *ptr = uuid; *ptr; ++ptr)
    if (!((*ptr >= 'a' && *ptr <= 'z') || (*ptr >= '0' && *ptr <= '9') || *ptr == '-'))
      die("illegal uuid");
  return uuid;
}

void useradd(int uid)
{
  if (!privileged)
    die("useradd not allowed without root privileges");
  char *cmdline;
  if (asprintf(&cmdline, "groupadd -g %1$d riju%1$d", uid) < 0)
    die("asprintf failed");
  int status = system(cmdline);
  if (status != 0)
    die("groupadd failed");
  if (asprintf(&cmdline, "useradd -M -N -l -r -u %1$d -g %1$d -p '!' -s /usr/bin/bash riju%1$d", uid) < 0)
    die("asprintf failed");
  status = system(cmdline);
  if (status != 0)
    die("useradd failed");
}

void spawn(int uid, char *uuid, char **cmdline)
{
  char *cwd;
  if (asprintf(&cwd, "/tmp/riju/%s", uuid) < 0)
    die("asprintf failed");
  if (chdir(cwd) < 0)
    die("chdir failed");
  if (privileged) {
    if (setgid(uid) < 0)
      die("setgid failed");
    if (setgroups(0, NULL) < 0)
      die("setgroups failed");
    if (setuid(uid) < 0)
      die("setuid failed");
  }
  umask(077);
  execvp(cmdline[0], cmdline);
  die("execvp failed");
}

void setup(int uid, char *uuid)
{
  char *cmdline;
  if (asprintf(&cmdline, privileged
               ? "install -d -o riju%1$d -g riju%1$d -m 700 /tmp/riju/%2$s"
               : "install -d -m 700 /tmp/riju/%2$s", uid, uuid) < 0)
    die("asprintf failed");
  int status = system(cmdline);
  if (status != 0)
    die("install failed");
}

void teardown(int uid, char *uuid)
{
  char *cmdline;
  int status;
  char *users;
  if (uid >= MIN_UID && uid < MAX_UID) {
    if (asprintf(&users, "%d", uid) < 0)
      die("asprintf failed");
  } else {
    cmdline = "getent passwd | grep -Eo '^riju[0-9]{4}' | paste -s -d, - | tr -d '\n'";
    FILE *fp = popen(cmdline, "r");
    if (fp == NULL)
      die("popen failed");
    static char buf[(MAX_UID - MIN_UID) * 9];
    if (fgets(buf, sizeof(buf), fp) == NULL) {
      if (feof(fp))
        users = NULL;
      else {
        die("fgets failed");
      }
    } else
      users = buf;
  }
  if (users != NULL) {
    if (asprintf(&cmdline, "pkill -SIGKILL --uid %s", users) < 0)
      die("asprintf failed");
    status = system(cmdline);
    if (status != 0 && status != 256)
      die("pkill failed");
  }
  if (asprintf(&cmdline, "rm -rf /tmp/riju/%s", uuid) < 0)
    die("asprintf failed");
  status = system(cmdline);
  if (status != 0)
    die("rm failed");
}

int main(int argc, char **argv)
{
  int code = setuid(0);
  if (code != 0 && code != -EPERM)
    die("setuid failed");
  privileged = code == 0;
  if (argc < 2)
    die_with_usage();
  if (!strcmp(argv[1], "useradd")) {
    if (argc != 3)
      die_with_usage();
    useradd(parseUID(argv[2]));
    return 0;
  }
  if (!strcmp(argv[1], "spawn")) {
    if (argc < 5)
      die_with_usage();
    spawn(parseUID(argv[2]), parseUUID(argv[3]), &argv[4]);
    return 0;
  }
  if (!strcmp(argv[1], "setup")) {
    if (argc != 4)
      die_with_usage();
    int uid = parseUID(argv[2]);
    char *uuid = parseUUID(argv[3]);
    setup(uid, uuid);
    return 0;
  }
  if (!strcmp(argv[1], "teardown")) {
    if (argc != 4)
      die_with_usage();
    int uid = strcmp(argv[2], "*") ? parseUID(argv[2]) : -1;
    char *uuid = strcmp(argv[3], "*") ? parseUUID(argv[3]) : "*";
    teardown(uid, uuid);
    return 0;
  }
  die_with_usage();
}
