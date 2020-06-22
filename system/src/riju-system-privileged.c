#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Keep in sync with backend/src/users.ts
const int MIN_UID = 2000;
const int MAX_UID = 65000;

void die(const char *msg)
{
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

void die_with_usage()
{
  die("usage:\n"
      "  riju-system-privileged useradd UID");
}

void useradd(int uid)
{
  char *cmdline;
  if (asprintf(&cmdline, "useradd -M -N -l -r -u %1$d riju_user%1$d", uid) < 0) {
    die("asprintf failed");
  }
  int status = system(cmdline);
  if (status) {
    die("useradd failed");
  }
}

int main(int argc, char **argv)
{
  setuid(0);
  if (argc < 2)
    die_with_usage();
  if (!strcmp(argv[1], "useradd")) {
    if (argc != 3)
      die_with_usage();
    char *endptr;
    long uid = strtol(argv[2], &endptr, 10);
    if (!argv[2] || *endptr) {
      die("uid must be an integer");
    }
    if (uid < MIN_UID || uid >= MAX_UID) {
      die("uid is out of range");
    }
    useradd(uid);
    return 0;
  }
  die_with_usage();
}
