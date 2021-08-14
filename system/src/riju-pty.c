#define _GNU_SOURCE

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

void __attribute__((noreturn)) die(char *msg)
{
  fprintf(stderr, "%s (errno %d)\n", msg, errno);
  exit(1);
}

void die_with_usage() { die("usage: riju-pty [-f] CMDLINE..."); }

struct termios orig_termios;

bool do_restore_tty = true;

void restore_tty()
{
  if (!do_restore_tty)
    return;
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios) < 0)
    die("tcsetattr failed");
}

int main(int argc, char **argv)
{
  argc -= 1;
  argv += 1;
  int no_pty = argc >= 1 && strcmp(argv[0], "-f") == 0;
  if (no_pty) {
    argc -= 1;
    argv += 1;
  }
  if (argc <= 0)
    die_with_usage();
  int pty_master_fd;
  char *pty_slave_name;
  if (!no_pty) {
    pty_master_fd = posix_openpt(O_RDWR);
    if (pty_master_fd < 0)
      die("posix_openpt failed");
    if (grantpt(pty_master_fd) < 0)
      die("grantpt failed");
    if (unlockpt(pty_master_fd) < 0)
      die("unlockpt failed");
    pty_slave_name = ptsname(pty_master_fd);
    if (pty_slave_name == NULL)
      die("ptsname failed");
  }
  if (isatty(STDIN_FILENO)) {
    if (tcgetattr(STDIN_FILENO, &orig_termios) < 0)
      die("tcgetattr failed");
    struct termios raw = orig_termios;
    cfmakeraw(&raw);
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) < 0)
      die("tcsetattr failed");
    if (atexit(restore_tty) < 0)
      die("atexit failed");
  } else {
    if (errno != ENOTTY)
      die("isatty failed");
  }
  pid_t orig_ppid = getpid();
  pid_t exec_pid = fork();
  if (exec_pid < 0)
    die("fork failed");
  else if (exec_pid == 0) {
    if (prctl(PR_SET_PDEATHSIG, SIGTERM) < 0)
      die("prctl failed");
    if (getppid() != orig_ppid)
      exit(EXIT_FAILURE);
    if (!no_pty) {
      close(pty_master_fd);
      if (setsid() < 0)
        die("setsid failed");
      int pty_slave_fd = open(pty_slave_name, O_RDWR);
      if (pty_slave_fd < 0)
        die("open failed");
      if (dup2(pty_slave_fd, STDIN_FILENO) < 0)
        die("dup2 failed");
      if (dup2(pty_slave_fd, STDOUT_FILENO) < 0)
        die("dup2 failed");
      if (dup2(pty_slave_fd, STDERR_FILENO) < 0)
        die("dup2 failed");
      if (close(pty_slave_fd) < 0)
        die("close failed");
    }
    execvp(argv[0], &argv[0]);
    die("execvp failed");
  }
  pid_t pid = no_pty ? 1 : fork();
  if (pid < 0)
    die("fork failed");
  else if (pid > 0) {
    int wstatus;
    if (waitpid(exec_pid, &wstatus, 0) != exec_pid)
      die("waitpid failed");
    if (!no_pty) {
      if (kill(-pid, SIGTERM) < 0)
        die("kill failed");
    }
    return WEXITSTATUS(wstatus);
  }
  if (prctl(PR_SET_PDEATHSIG, SIGTERM) < 0)
    die("prctl failed");
  if (getppid() != orig_ppid)
    exit(EXIT_FAILURE);
  do_restore_tty = false;
  if (setpgrp() < 0)
    die("setpgrp failed");
  char buf[1024];
  int len, len_written;
  orig_ppid = getpid();
  pid = fork();
  if (pid < 0)
    die("fork failed");
  else if (pid == 0) {
    if (prctl(PR_SET_PDEATHSIG, SIGTERM) < 0)
      die("prctl failed");
    if (getppid() != orig_ppid)
      exit(EXIT_FAILURE);
    while ((len = read(STDIN_FILENO, buf, 1024)) > 0) {
      char *ptr = buf;
      while (len > 0) {
        switch (*ptr) {
        case 3:
          if (kill(exec_pid, SIGINT) < 0)
            die("kill failed");
          len -= 1;
          ptr += 1;
          continue;
        }
        int limit = len;
        for (int idx = 0; idx < len; ++idx) {
          if (buf[idx] == 3) {
            limit = idx;
            break;
          }
        }
        len_written = write(pty_master_fd, ptr, limit);
        if (len_written < 0)
          die("write failed");
        len -= len_written;
        ptr += len_written;
      }
    }
    if (len < 0) {
      // ignore read failure
    }
  } else {
    if (setvbuf(stdout, NULL, _IONBF, 0) != 0)
      die("setvbuf failed");
    while ((len = read(pty_master_fd, buf, 1024)) > 0) {
      fwrite(buf, 1, len, stdout);
      if (ferror(stdout))
        die("fwrite failed");
      if (feof(stdout))
        break;
    }
    if (len < 0) {
      // ignore read failure
    }
  }
  return 0;
}
