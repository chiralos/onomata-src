#include "ono.h"
#include "env.h"
#include <fcntl.h>
#include <unistd.h>
#include <termios.h>
#include <stdio.h> // for snprintf
#include <string.h> // for strlen
#include <fcntl.h>

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

void readCode(void) {
  Word  len = env.sp[-1];
  char* buf = (char *)(env.sp[-2]);
  popCode();
  Int fd  = popInt();
  pushInt(read(fd,buf,len));
}

void writeCode(void) {
  Word* b  = stack2();
  void* buf;
  size_t len = env.sp[-1];
  if (env.sp[0] == TAG_BYTES)
    buf = (void *)itemBase(env.sp);
  else
    buf = (void *)(env.sp[-2]);
  Int fd = b[-1];
  Int r  = write(fd,buf,len);
  env.sp = b;
  b[-1]  = r; // NOTE convert to onomata IO error code
}

void termclsCode(void) {
  int fd = popInt();
  pushInt(ioWrite(fd,"\x1b[1J",4));
}

void termcursortoCode(void) {
  int y  = popInt();
  int x  = popInt();
  int fd = popInt();
  if (y < 0 || x < 0) {
    popCode(); pushInt(-1);
  } else {
    char buf[64];
    snprintf(buf,64,"\x1b[%d;%dH",y+1,x+1);
    pushInt(ioWrite(fd,buf,strlen(buf)));
  }
}

void termmodeCode(void) {
  int fd = popInt();
  int r = 0;
  if (!isatty(fd)) {
    r = -1; // see [1]
  } else {
    Byte op = env.pc[0];
    struct termios attr;
    tcgetattr(fd, &attr);
    attr.c_lflag &= ~(ICANON | ECHO);
    if (op == OP_TERMRESET)
      attr.c_lflag |= (ICANON | ECHO);
    r = tcsetattr(fd, TCSANOW, &attr);

    if (op == OP_TERMRAWNONBLOCKING || op == OP_TERMRESET) {
      int flstate = fcntl(fd,F_GETFL,0);
      flstate &= ~O_NONBLOCK;
      if (op == OP_TERMRAWNONBLOCKING)
        flstate = flstate | O_NONBLOCK;
      int r2 = fcntl(fd,F_SETFL,flstate);
      if (r == 0) r = r2;
    }
  }
  pushInt(r);
}

////////
// Notes
/*
[1] We should probably try to do better than this.
    byte because we just popped an int.
*/
