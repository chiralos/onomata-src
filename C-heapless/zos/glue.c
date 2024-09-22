#include <zos_sys.h>
#include <zos_vfs.h>
#include <stdio.h>

#define MAX_FILES 8

void abort(char* msg);

#define BUFSIZE 8192
uint8_t buffer[BUFSIZE];

int main(int inArgc, char** inArgv) {
  if (inArgc < 1) abort("");
  char *argv[MAX_FILES];
  int argc = 0;
  char *p = inArgv[0];
  while (*p) {
    if (*p == ' ') {
      while (*p == ' ') p++;
    } else {
      if (argc >= MAX_FILES)
        abort("too many files");
      argv[argc++] = p;
      while (*p != ' ' && *p !=  '\0') p++;
      if (*p) {
        *p = '\0';
        p++;
      }
    }
  }

  if (argc < 2) abort("");

  zos_dev_t tfd = open(argv[0],O_WRONLY | O_APPEND);
  if (tfd < 0) abort("could not open target");
  int i;
  for (i=1;i < argc;i++) {
    zos_dev_t sfd = open(argv[i],O_RDONLY);
    if (sfd < 0) abort("could not open source");
    uint16_t len = 0;
    while (1) {
      len = BUFSIZE;
      zos_err_t err = read(sfd,buffer,&len);
      if (err) abort("read error");
      if (len == 0) break;
      err = write(tfd,buffer,&len);
      if (err) abort("write error");
    }
    close(sfd);
  }
  close(tfd);
  return 0;
}

void abort(char* msg) {
  if (*msg != '\0')
    printf("%s\n",msg);
  printf("usage: glue FILE1 [FILE2 ...]\n");
  printf("       FILE1 is modified with other files appended.\n");
  exit(1);
}
