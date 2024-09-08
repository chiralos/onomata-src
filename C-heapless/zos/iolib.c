#include "ono.h"
#include "env.h"

#include <zos_sys.h>
#include <zos_time.h>
#include <zos_vfs.h>
#include <zos_keyboard.h>
#include <zos_video.h>

void sleepmilliCode(void) {
  Int dt = popInt();
  if (dt < 0) THROW(ERR_BAD_ARGUMENT);
  msleep((uint16_t)dt);
}

static zos_err_t longIO(bool readMode, zos_dev_t dev, void* buf, uint16_t* sizen);

Int ioWrite (int fd, char *s, Word len) {
  if (len) 
    (void)longIO(false,fd,(void *)s,&len);
  return len;
}

const int stdinFD = 1;
const int stdoutFD = 0;

void openCode(void) {
  Int flags = popInt();
  char *fName = (char *)itemBase(env.sp);
  Int nameLen = env.sp[-1];
  popCode();
  fName[nameLen] = '\0';
  Int fd = open(fName,flags);
  pushInt(fd);
}

void closeCode(void) {
  Int fd = popInt();
  Int r = close(fd);
  pushInt(r); // NOTE convert to onomata error code
}

void seekCode(void) {
  zos_whence_t whence = (zos_whence_t)popInt();
  int32_t      offset = (int32_t)popInt();
  Int          fd     = popInt();
  zos_err_t err = seek(fd,&offset,whence);
  pushInt(err > 0 ? -err : ((Int)offset));
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

static zos_err_t longIO(bool readMode, zos_dev_t dev, void* buf, uint16_t* sizen) {
  uint16_t end      = ((uint16_t)buf) + *sizen;
  uint16_t startSeg = ((uint16_t)buf) >> 14;
  uint16_t endSeg   = ((uint16_t)end) >> 14;
  uint16_t totalLen = 0;
  uint16_t len;
  zos_err_t err = ERR_SUCCESS;
  while (startSeg < endSeg) {
    startSeg++;
    void* bufEnd = (void *)(startSeg << 14);
    len = (uint16_t)(bufEnd - buf);
    uint16_t rqLen = len;
    err = readMode ? read(dev,buf,&len) : write(dev,buf,&len);
    if (err != ERR_SUCCESS) return err;
    totalLen += len;
    if (len < rqLen) {
      *sizen = totalLen;
      return ERR_SUCCESS;
    }
    buf = bufEnd;
  }
  len = end - ((uint16_t)buf);
  err = readMode ? read(dev,buf,&len) : write(dev,buf,&len);
  if (err == ERR_SUCCESS)
    *sizen = totalLen + len;
  return err;
}

void readstrCode(void) {
  Int len = popInt();
  Int fd  = popInt();
  overflowCheck(alignedSize(len + 2));
  zos_err_t err = longIO(true,fd,(char *)(env.sp+1),&len);
  Int r = 0;
  if (err != ERR_SUCCESS) {
    *(++env.sp) = 0;
    r = err;
  } else {
    Word sz = alignedSize(len);
    env.sp += sz;
    *(++env.sp) = len;
    r = len;
  }
  *(++env.sp) = TAG_BYTES;
  pushInt(r);
}

void writestrCode(void) {
  void* buf;
  Word len;
  Word* b  = stack2();
  buf = (void *)itemBase(env.sp);
  len = env.sp[-1];
  Int fd   = b[-1];
  zos_err_t err = longIO(false,fd,buf,&len);
  env.sp = b;
  b[-1] = (Int) err; // NOTE convert to std err code
}

void termclsCode(void) {
  int fd = popInt(); // see [2]
  pushInt(ioctl(fd,CMD_CLEAR_SCREEN,0));
}

void termcursortoCode(void) {
  int y  = popInt();
  int x  = popInt();
  int fd = popInt();
  if (y < 0 || x < 0) {
    popCode(); pushInt(-1);
  } else {
    pushInt(ioctl(fd,CMD_SET_CURSOR_XY,
      (void *)((x << 8) | (y & 0xff))));
  }
}

void termmodeCode(void) {
  int fd = popInt(); // see [1]
  int r = 0;
  Word x = 0;
  switch (env.pc[0]) {
    case OP_TERMRAW:
      x = KB_MODE_RAW;
      break;
    case OP_TERMRAWNONBLOCKING:
      x = KB_MODE_RAW | KB_READ_NON_BLOCK;
      break;
    case OP_TERMRESET:
      x = KB_MODE_COOKED | KB_READ_BLOCK;
      break;
  }
  pushInt(ioctl(fd,KB_CMD_SET_MODE,(void *)x));
}

////////   
// Notes
/*
[1] As far as I know there is no way to know if a fd is
    connected to a particular device. We just trust the
user only calls this on the keyboard device. We could check
that the fd is stdin, but that fails if the keyboad fd is
cloned or stdin is not connected to the keyboard.

Also, here is no KB_CMD_GET_MODE, so we can't separately
control raw/cooked and nonblocking.
*/
