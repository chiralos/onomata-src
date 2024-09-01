#include "ono.h"
#include "env.h"
#include <fcntl.h>
#include <unistd.h>
#include <termios.h>

void sleepmilliCode(void) {
  Int dt = popInt();
  if (dt < 0) THROW(ERR_BAD_ARGUMENT);
  usleep(dt*1000);
}

Int ioWrite(int fd, char *s, Word len) { return write(fd,s,len); }

const int stdinFD = 0;
const int stdoutFD = 1;

void openCode(void) {
  Int flags      = popInt();
  char *fName    = (char *)itemBase(env.sp);
  Int nameLen    = env.sp[-1];
  popCode();
  fName[nameLen] = '\0'; // see [1]
  Int fd         = open(fName,flags,0664);
  pushInt(fd);
}

void closeCode(void) {
  Int fd = popInt();
  Int r = close(fd);
  pushInt(r);
}

void seekCode(void) {
  Int whence   = popInt();
  off_t offset = (off_t)popInt();
  Int fd       = popInt();
  pushInt((Int)lseek(fd,(off_t)offset,whence));
}

#define INTPRIM(NAME,NUM) void NAME(void) { pushInt(NUM); }

INTPRIM(seeksetCode,SEEK_SET)
INTPRIM(seekcurCode,SEEK_CUR)
INTPRIM(seekendCode,SEEK_END)
INTPRIM(rdonlyCode,O_RDONLY)
INTPRIM(wronlyCode,O_WRONLY)
INTPRIM(rdwrCode,O_RDWR)
INTPRIM(truncCode,O_TRUNC)
INTPRIM(creatCode,O_CREAT)
INTPRIM(nonblockCode,O_NONBLOCK)

void readstrCode(void) {
  Int len = popInt();
  Int fd  = popInt();
  overflowCheck(alignedSize(len + 2));
  Int r = read(fd,(char *)(env.sp+1),len);
  if (r < 0) {
    *(++env.sp) = 0;
    // NOTE convert to onomata IO error code ?
  } else {
    Word sz = alignedSize(r);
    env.sp += sz;
    *(++env.sp) = r;
  }
  *(++env.sp) = TAG_BYTES;
  pushInt(r);
}

void writestrCode(void) {
  void* buf;
  size_t len;
  Word* b  = stack2();
  buf = (void *)itemBase(env.sp);
  len = env.sp[-1];
  Int fd   = b[-1];
  Int r    = write(fd,buf,len);
  env.sp   = b;
  b[-1]    = r; // NOTE convert to onomata IO error code
}

void setrawCode(void) {
  bool set = env.sp[0] == TAG_TRUE;
  env.sp--;
  int fd = popInt();
  struct termios attr;
  tcgetattr(fd, &attr);
  attr.c_lflag &= ~(ICANON | ECHO);
  if (!set)
    attr.c_lflag |= (ICANON | ECHO);
  int r = tcsetattr(fd, TCSANOW, &attr);
  pushInt(r);
}

////////
// Notes
/*
  [1] We know there is space for the null-termnating
      byte because we just popped an int.
*/
