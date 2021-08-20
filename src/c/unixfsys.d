/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * unixfsys.d - Unix file system interface
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <string.h>
#include <stdio.h>
#include <limits.h>

#ifndef _MSC_VER
# include <unistd.h>
#else
# include <io.h>
# include <direct.h>
# define F_OK 0
typedef int mode_t; 
#endif

#include <sys/types.h>
#include <ecl/ecl.h>
#ifdef HAVE_PWD_H
# include <pwd.h>
#endif
#include <sys/stat.h>
#include <stdlib.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#ifdef HAVE_DIRENT_H
# include <dirent.h>
#else
# if !defined(_MSC_VER)
#  include <sys/dir.h>
# endif
#endif
#if defined(ECL_MS_WINDOWS_HOST)
# include <windows.h>
# undef ERROR
#endif
#include <fcntl.h>
#include <errno.h>

#if defined(ECL_MS_WINDOWS_HOST) && defined(ECL_UNICODE)
cl_object
ecl_make_simple_filename(const ecl_filename_char *x, cl_fixnum size)
{
  if (size < 0) {
    size = wcslen(x);
  }
  cl_object vec = si_make_vector(@'ext::byte16', /* element-type */
                                 ecl_make_fixnum(size+1), /* size */
                                 ECL_NIL,               /* adjustable */
                                 ECL_NIL,               /* fillp */
                                 ECL_NIL,               /* displaced */
                                 ECL_NIL);              /* displaced-offset */
  for (cl_index i = 0; i < size; i++) {
    vec->vector.self.b16[i] = x[i];
  }
  vec->vector.self.b16[size] = 0;
  /* use the same trick as for base strings to store the null
   * terminator: allocate one element more, adjust fill-pointer and
   * dimension */
  vec->vector.fillp = vec->vector.dim = size;
  return vec;
}

cl_object
ecl_concatenate_filename(cl_object x, cl_object y)
{
  cl_index size = x->vector.fillp + y->vector.fillp;
  cl_object vec = si_make_vector(@'ext::byte16', /* element-type */
                                 ecl_make_fixnum(size+1), /* size */
                                 ECL_NIL,               /* adjustable */
                                 ECL_NIL,               /* fillp */
                                 ECL_NIL,               /* displaced */
                                 ECL_NIL);              /* displaced-offset */
  if (x->vector.elttype == ecl_aet_bc) {
    for (cl_index i = 0; i < x->vector.fillp; i++) {
      vec->vector.self.b16[i] = x->vector.self.bc[i];
    }
  } else if (x->vector.elttype == ecl_aet_b16) {
    for (cl_index i = 0; i < x->vector.fillp; i++) {
      vec->vector.self.b16[i] = x->vector.self.b16[i];
    }
  } else {
    ecl_internal_error("Wrong type for first argument to ecl_concatenate_filename");
  }
  if (y->vector.elttype == ecl_aet_bc) {
    for (cl_index i = 0; i < y->vector.fillp; i++) {
      vec->vector.self.b16[i + x->vector.fillp] = y->vector.self.bc[i];
    }
  } else if (y->vector.elttype == ecl_aet_b16) {
    for (cl_index i = 0; i < y->vector.fillp; i++) {
      vec->vector.self.b16[i + x->vector.fillp] = y->vector.self.b16[i];
    }
  } else {
    ecl_internal_error("Wrong type for second argument to ecl_concatenate_filename");
  }
  vec->vector.self.b16[size] = 0;
  vec->vector.fillp = vec->vector.dim = size;
  return vec;
}

cl_object
ecl_alloc_filename(cl_index len, cl_object adjustable)
{
  cl_object ret = si_make_vector(@'ext::byte16', /* element-type */
                                 ecl_make_fixnum(len+1),/* size */
                                 adjustable,            /* adjustable */
                                 ecl_make_fixnum(0),    /* fillp */
                                 ECL_NIL,               /* displaced */
                                 ECL_NIL);              /* displaced-offset */
  ret->vector.self.b16[len] = 0;
  ret->vector.fillp = ret->vector.dim = len;
  return ret;
}

cl_object
ecl_decode_filename(cl_object x, cl_object len)
{
  return si_octets_to_string(5, x,
                             @':end', len,
                             @':external-format', @':ucs-2');
}

cl_object
ecl_encode_filename(cl_object x, cl_object len)
{
  return si_string_to_octets(9, x,
                             @':end', len,
                             @':null-terminate', ECL_T,
                             @':element-type', @'ext::byte16',
                             @':external-format', @':ucs-2');
}

#else
cl_object
ecl_decode_filename(cl_object x, cl_object len)
{
  return si_octets_to_string(3, x, @':end', len);
}

cl_object
ecl_encode_filename(cl_object x, cl_object len)
{
  return si_string_to_octets(7, x,
                             @':end', len,
                             @':null-terminate', ECL_T,
                             @':element-type', @'base-char');
}
#endif

static int
safe_chdir(const ecl_filename_char *path, cl_object prefix)
{
  if (prefix != ECL_NIL) {
    cl_object aux = ecl_make_constant_filename(path,-1);
    aux = ecl_concatenate_filename(prefix, aux);
    return safe_chdir(ecl_filename_self(aux), ECL_NIL);
  } else {
    int output;
    ecl_disable_interrupts();
    output = ecl_chdir((ecl_filename_char *)path);
    ecl_enable_interrupts();
    return output;
  }
}

static int
safe_stat(const ecl_filename_char *path, ecl_stat_struct *sb)
{
  int output;
  ecl_disable_interrupts();
  output = ecl_stat(path, sb);
  ecl_enable_interrupts();
  return output;
}

#ifdef HAVE_LSTAT
static int
safe_lstat(const ecl_filename_char *path, ecl_stat_struct *sb)
{
  int output;
  ecl_disable_interrupts();
  output = lstat(path, sb);
  ecl_enable_interrupts();
  return output;
}
#endif

/*
 * Finds current directory by using getcwd() with an adjustable
 * string which grows until it can host the whole path.
 */
static cl_object
current_dir(void) {
  ecl_filename_char *output;
  const ecl_filename_char *ok;
#ifdef _MSC_VER
  ecl_filename_char *c;
#endif
  cl_index size = 128;

  do {
    output = ecl_alloc_atomic((size+2)*sizeof(ecl_filename_char));
    ecl_disable_interrupts();
    ok = ecl_getcwd(output, size);
    if (ok == NULL && errno != ERANGE) {
      perror("ext::getcwd error");
      ecl_internal_error("Can't work without CWD");
    }
    ecl_enable_interrupts();
    size += 256;
  } while (ok == NULL);
  size = ecl_fstrlen(output);
#ifdef _MSC_VER
  for (c = output; *c; c++) {
    if (*c == '\\') *c = '/';
  }
#endif
  if (output[size-1] != '/') {
    output[size++] = '/';
    output[size] = '\0';
  }
  return ecl_make_constant_filename(output, size);
}

/*
 * Using a certain path, guess the type of the object it points to.
 */

static cl_object
file_kind(ecl_filename_char *filename, bool follow_links) {
  cl_object output;
#if defined(ECL_MS_WINDOWS_HOST)
  DWORD dw;
  ecl_disable_interrupts();
  dw = ecl_GetFileAttributes( filename );
  if (dw == -1)
    output = ECL_NIL;
  else if ( dw & FILE_ATTRIBUTE_DIRECTORY )
    output = @':directory';
  else
    output = @':file';
  ecl_enable_interrupts();
#else
  ecl_stat_struct buf;
# ifdef HAVE_LSTAT
  if ((follow_links? safe_stat : safe_lstat)(filename, &buf) < 0)
# else
    if (safe_stat(filename, &buf) < 0)
# endif
      output = ECL_NIL;
# ifdef HAVE_LSTAT
    else if (S_ISLNK(buf.st_mode))
      output = @':link';
# endif
    else if (S_ISDIR(buf.st_mode))
      output = @':directory';
    else if (S_ISREG(buf.st_mode))
      output = @':file';
# ifdef S_ISFIFO
    else if (S_ISFIFO(buf.st_mode))
      output = @':fifo';
# endif
    else
      output = @':special';
#endif
  return output;
}

cl_object
si_file_kind(cl_object filename, cl_object follow_links) {
  filename = si_coerce_to_filename(filename);
  @(return file_kind(ecl_filename_self(filename), !Null(follow_links)));
}

#if defined(HAVE_LSTAT) && !defined(ECL_MS_WINDOWS_HOST)
static cl_object
si_readlink(cl_object filename) {
  /* Given a filename which is a symlink, this routine returns
   * the value of this link in the form of a pathname. */
  cl_index size = 128, written;
  char *output;
  cl_object kind;
  do {
    /* We reserve 2 characters for trailing '/' and '\0' */
    output = ecl_alloc_atomic(size+2);
    ecl_disable_interrupts();
    written = readlink(ecl_filename_self(filename), output, size);
    ecl_enable_interrupts();
    size += 256;
  } while (written == size-256);
  output[written] = '\0';
  kind = file_kind(output, FALSE);
  if (kind == @':directory') {
    output[written++] = '/';
    output[written] = '\0';
  }
  return ecl_decode_filename(ecl_make_constant_filename(output, written), ECL_NIL);
}
#endif /* HAVE_LSTAT */

static cl_object
enter_directory(cl_object base_dir, cl_object subdir, bool ignore_if_failure)
{
  /* Assuming we start in "base_dir", enter a subdirectory named by
   * "subdir", which may be a string, :UP, :ABSOLUTE or :RELATIVE.
   * If the operation succeeds, return the truename of the resulting
   * path -- resolving any links in the process. */
  cl_object aux, output, kind;
  if (subdir == @':absolute') {
    return cl_make_pathname(4, @':directory', ecl_list1(subdir),
                            @':defaults', base_dir);
  } else if (subdir == @':relative') {
    /* Nothing to do */
    return base_dir;
  } else if (subdir == @':up') {
    aux = @"..";
  } else {
    aux = subdir;
  }
  /* We now compose a new path based on the base directory and
   * the new component. We have to verify that the new pathname is
   * a directory and if it is a link recover the true name. */
  aux = ecl_append(base_dir->pathname.directory, ecl_list1(aux));
  output = cl_make_pathname(4, @':directory', aux, @':defaults', base_dir);
  aux = ecl_namestring(output, ECL_NAMESTRING_FORCE_BASE_STRING);
  /* We remove the trailing '/' from the namestring because the
   * POSIX library does not like it. */
  ecl_filename_self(aux)[--aux->base_string.fillp] = 0;
  kind = file_kind(ecl_filename_self(aux), FALSE);
  if (kind == ECL_NIL) {
    if (ignore_if_failure) return ECL_NIL;
    FEcannot_open(output);
#ifdef HAVE_LSTAT
  } else if (kind == @':link') {
    output = cl_truename(ecl_merge_pathnames(si_readlink(aux),
                                             base_dir, @':default'));
    if (output->pathname.name != ECL_NIL ||
        output->pathname.type != ECL_NIL)
      goto WRONG_DIR;
    return output;
#endif
  } else if (kind != @':directory') {
  WRONG_DIR:
    if (ignore_if_failure) return ECL_NIL;
    FEerror("The directory~&  ~S~&in pathname~&  ~S~&"
            "actually points to a file or special device.",
            2, subdir, base_dir);
  }
  if (subdir == @':up') {
    cl_object newdir= output->pathname.directory;
    newdir = ecl_nbutlast(newdir, 2);
    if (Null(newdir)) {
      if (ignore_if_failure) return ECL_NIL;
      FEerror("Pathname contained an :UP component  "
              "that goes above the base directory:"
              "~&  ~S", 1, output);
    }
    output->pathname.directory = newdir;
  }
  return output;
}

static cl_object
make_absolute_pathname(cl_object orig_pathname)
{
  /* INV: si_coerce_to_file_pathname creates an absolute pathname */
  return si_coerce_to_file_pathname(orig_pathname);
}

static cl_object
make_base_pathname(cl_object pathname)
{
  return ecl_make_pathname(pathname->pathname.host,
                           pathname->pathname.device,
                           ecl_list1(@':absolute'),
                           ECL_NIL, ECL_NIL, ECL_NIL, @':local');
}

#define FOLLOW_SYMLINKS 1

static cl_object
file_truename(cl_object pathname, cl_object filename, int flags)
{
  cl_object kind;
  if (Null(pathname)) {
    if (Null(filename)) {
      ecl_internal_error("file_truename:"
                         " both FILENAME and PATHNAME are null!");
    }
    pathname = cl_pathname(filename);
  } else if (Null(filename)) {
    filename = ecl_namestring(pathname, ECL_NAMESTRING_FORCE_BASE_STRING);
    if (Null(filename)) {
      FEerror("Unprintable pathname ~S found in TRUENAME", 1, pathname);
    }
  }
  kind = file_kind(ecl_filename_self(filename), FALSE);
  if (kind == ECL_NIL) {
    FEcannot_open(pathname);
#ifdef HAVE_LSTAT
  } else if (kind == @':link' && (flags & FOLLOW_SYMLINKS)) {
    /* The link might be a relative pathname. In that case
     * we have to merge with the original pathname.  On
     * the other hand, if the link is broken – return file
     * truename "as is". */
    ecl_stat_struct filestatus;
    if (safe_stat(ecl_filename_self(filename), &filestatus) < 0) {
      @(return pathname kind);
    }
    filename = si_readlink(filename);
    pathname = ecl_make_pathname(pathname->pathname.host,
                                 pathname->pathname.device,
                                 pathname->pathname.directory,
                                 ECL_NIL, ECL_NIL, ECL_NIL, @':local');
    pathname = ecl_merge_pathnames(filename, pathname, @':default');
    return cl_truename(pathname);
#endif
  } else if (kind == @':directory'){
    /* If the pathname is a directory but we have supplied
       a file name, correct the type by appending a directory
       separator and re-parsing again the namestring */
    if (pathname->pathname.name != ECL_NIL ||
        pathname->pathname.type != ECL_NIL) {
      pathname = ecl_concatenate_filename(filename, @"/");
      pathname = ecl_decode_filename(pathname, ECL_NIL);
      pathname = cl_truename(pathname);
    }
  }
  /* ECL does not contemplate version numbers
     in directory pathnames */
  if (pathname->pathname.name == ECL_NIL &&
      pathname->pathname.type == ECL_NIL) {
    /* We have to destructively change the
     * pathname version here. Otherwise
     * merge_pathnames will not do it. It is
     * safe because coerce_to_file_pathname
     * created a copy. */
    pathname->pathname.version = ECL_NIL;
  } else {
    pathname->pathname.version = @':newest';
  }
  @(return pathname kind);
}

/*
 * Search the actual name of the directory of a pathname,
 * going through links if they exist. Default is
 * current directory
 */
cl_object
cl_truename(cl_object orig_pathname)
{
  cl_object pathname = make_absolute_pathname(orig_pathname);
  cl_object base_dir = make_base_pathname(pathname);
  cl_object dir;
  /* We process the directory part of the filename, removing all
   * possible symlinks. To do so, we inspect recursively the
   * directory which contains our file, and come back. We also have to
   * ensure that the filename itself does not point to a symlink: if so,
   * then we resolve the value of the symlink and continue traversing
   * the filesystem.
   */
  for (dir = pathname->pathname.directory; !Null(dir); dir = ECL_CONS_CDR(dir))
    {
      base_dir = enter_directory(base_dir, ECL_CONS_CAR(dir), 0);
    }
  pathname = ecl_merge_pathnames(base_dir, pathname, @':default');
  @(return file_truename(pathname, ECL_NIL, FOLLOW_SYMLINKS));
}

int
ecl_backup_open(const ecl_filename_char *filename, int option, int mode)
{
  cl_index length = ecl_fstrlen(filename);
  ecl_filename_char *backupfilename = ecl_alloc_atomic((length + 5)*sizeof(ecl_filename_char));
  if (backupfilename == NULL) {
    FElibc_error("Cannot allocate memory for backup filename", 0);
  }

  ecl_fstrcat(ecl_fstrcpy(backupfilename, filename), ecl_fstr(".BAK"));
  ecl_disable_interrupts();
#if defined(ECL_MS_WINDOWS_HOST)
  /* Windows' rename doesn't replace an existing file */
  if (ecl_access(backupfilename, F_OK) == 0 && ecl_unlink(backupfilename)) {
    ecl_enable_interrupts();
    FElibc_error("Cannot remove the file ~S", 1,
                 ecl_decode_filename(ecl_make_constant_filename(backupfilename,-1), ECL_NIL));
  }
#endif
  if (ecl_rename(filename, backupfilename)) {
    ecl_enable_interrupts();
    FElibc_error("Cannot rename the file ~S to ~S.", 2,
                 ecl_decode_filename(ecl_make_constant_filename(filename,-1), ECL_NIL),
                 ecl_decode_filename(ecl_make_constant_filename(backupfilename,-1), ECL_NIL));
  }
  ecl_enable_interrupts();
  ecl_dealloc(backupfilename);
  return ecl_open(filename, option, mode);
}

cl_object
ecl_file_len(int f)
{
  ecl_stat_struct filestatus;
  memset(&filestatus, 0, sizeof(filestatus));
  ecl_disable_interrupts();
  ecl_fstat(f, &filestatus);
  ecl_enable_interrupts();
#ifdef S_ISFIFO
  if (S_ISFIFO(filestatus.st_mode)) {
    return ECL_NIL;
  } else {
    return ecl_make_integer(filestatus.st_size);
  }
#else
  return ecl_make_integer(filestatus.st_size);
#endif
}

@(defun rename-file (oldn newn &key (if_exists @':error'))
  cl_object old_filename, new_filename, old_truename, new_truename;
#if defined(ECL_MS_WINDOWS_HOST)
  int error;
#endif
  @

  /* 1) Get the old filename, and complain if it has wild components,
   *    or if it does not exist. Notice that the filename to be renamed
   *    is not the truename, because we might be renaming a symbolic link.
   */
  old_truename = cl_truename(oldn);
  old_filename = si_coerce_to_filename(old_truename);

  /* 2) Create the new file name. */
  newn = ecl_merge_pathnames(newn, oldn, @':newest');
  new_filename = si_coerce_to_filename(newn);

  while (if_exists == @':error' || if_exists == ECL_NIL)
    {
      if (file_kind(ecl_filename_self(new_filename), TRUE) == ECL_NIL) {
        if_exists = ECL_T;
        break;
      }
      /* if the file already exists */
      if (if_exists == @':error') {
        const char *msg = "When trying to rename ~S, ~S already exists";
        if_exists =
          si_signal_simple_error
          (6, @'file-error', /* condition */
           @':supersede', /* continuable */
           /* format */
           ecl_make_constant_base_string(msg,strlen(msg)),
           cl_list(2, oldn, new_filename), /* format args */
           @':pathname', /* file-error options */
           new_filename);
        if (if_exists == ECL_T) if_exists= @':error';
      }
      if (if_exists == ECL_NIL) {
        @(return ECL_NIL ECL_NIL ECL_NIL);
      }
    }
  if (ecl_unlikely(if_exists != @':supersede' && if_exists != ECL_T)) {
    /* invalid key */
    FEerror("~S is an illegal IF-EXISTS option for RENAME-FILE.",
            1, if_exists);
  }
  {
    ecl_disable_interrupts();
#if defined(ECL_MS_WINDOWS_HOST)
    error = SetErrorMode(0);
    if (ecl_MoveFile(ecl_filename_self(old_filename),
                     ecl_filename_self(new_filename))) {
      SetErrorMode(error);
      goto SUCCESS;
    }
    switch (GetLastError()) {
    case ERROR_ALREADY_EXISTS:
    case ERROR_FILE_EXISTS:
      break;
    default:
      goto FAILURE_CLOBBER;
    };
    if (ecl_MoveFileEx(ecl_filename_self(old_filename),
                       ecl_filename_self(new_filename),
                       MOVEFILE_REPLACE_EXISTING)) {
      SetErrorMode(error);
      goto SUCCESS;
    }
    /* fallback on old behavior */
    ecl_DeleteFile(ecl_filename_self(new_filename));
    if (ecl_MoveFile(ecl_filename_self(old_filename),
                     ecl_filename_self(new_filename))) {
      SetErrorMode(error);
      goto SUCCESS;
    }
    /* fall through */
#else
    if (ecl_rename(ecl_filename_self(old_filename),
                   ecl_filename_self(new_filename)) == 0) {
      goto SUCCESS;
    }
#endif
  }
#if defined(ECL_MS_WINDOWS_HOST)
 FAILURE_CLOBBER:
#endif
  ecl_enable_interrupts();
  {
    cl_object c_error = _ecl_strerror(errno);
    const char *msg = "Unable to rename file ~S to ~S.~%C library error: ~S";
    si_signal_simple_error
      (6, @'file-error', /* condition */
       ECL_NIL, /* continuable */
       ecl_make_constant_base_string(msg,strlen(msg)), /* format */
       cl_list(3, oldn, newn, c_error), /* format args */
       @':pathname', /* file-error options */
       oldn);
  }

 SUCCESS:
  ecl_enable_interrupts();
  new_truename = cl_truename(newn);
  @(return newn old_truename new_truename);
  @)

static int
directory_pathname_p(cl_object path)
{
  return (path->pathname.name == ECL_NIL) &&
    (path->pathname.type == ECL_NIL);
}

cl_object
cl_delete_file(cl_object file)
{
  cl_object path = cl_pathname(file);
  int isdir = directory_pathname_p(path);
  cl_object filename = si_coerce_to_filename(path);
  int ok;

  ecl_disable_interrupts();
  ok = (isdir? ecl_rmdir : ecl_unlink)(ecl_filename_self(filename));
  ecl_enable_interrupts();

  if (ok < 0) {
    const char *msg =
      isdir?
      "Cannot delete the directory ~S.~%C library error: ~S" :
      "Cannot delete the file ~S.~%C library error: ~S";
    cl_object c_error = _ecl_strerror(errno);
    si_signal_simple_error
      (6, @'file-error', /* condition */
       ECL_T, /* continuable */
       ecl_make_constant_base_string(msg,strlen(msg)), /* format */
       cl_list(2, file, c_error), /* format args */
       @':pathname', /* file-error options */
       file);
  }
  @(return ECL_T);
}

cl_object
cl_probe_file(cl_object file)
{
  /* INV: Both SI:FILE-KIND and TRUENAME complain if "file" has wildcards */
  @(return (si_file_kind(file, ECL_T) != ECL_NIL? cl_truename(file) : ECL_NIL));
}

cl_object
cl_file_write_date(cl_object file)
{
  cl_object time, filename = si_coerce_to_filename(file);
  ecl_stat_struct filestatus;
  if (safe_stat(ecl_filename_self(filename), &filestatus) < 0) {
    time = ECL_NIL;
  } else {
    time = UTC_time_to_universal_time(filestatus.st_mtime);
  }
  @(return time);
}

cl_object
cl_file_author(cl_object file)
{
  cl_object output, filename = si_coerce_to_filename(file);
  ecl_stat_struct filestatus;
  if (safe_stat(ecl_filename_self(filename), &filestatus) < 0) {
    const char *msg = "Unable to read file author for ~S."
      "~%C library error: ~S";
    cl_object c_error = _ecl_strerror(errno);
    si_signal_simple_error
      (6, @'file-error', /* condition */
       ECL_T, /* continuable */
       ecl_make_constant_base_string(msg,strlen(msg)), /* format */
       cl_list(2, file, c_error), /* format args */
       @':pathname', /* file-error options */
       file);
  }
#ifdef HAVE_PWD_H
  {
    struct passwd *pwent;
    ecl_disable_interrupts();
    pwent = getpwuid(filestatus.st_uid);
    ecl_enable_interrupts();
    output = ecl_make_simple_base_string(pwent->pw_name,-1);
    output = si_octets_to_string(1, output);
  }
#else
  output = @"UNKNOWN";
#endif
  @(return output);
}

cl_object
ecl_homedir_pathname(cl_object user)
{
  cl_index i;
  cl_object namestring;
  const ecl_filename_char *h;
#if defined(ECL_MS_WINDOWS_HOST)
  const ecl_filename_char *d;
#endif
  if (!Null(user)){
#ifdef HAVE_PWD_H
    struct passwd *pwent = NULL;
#endif
    ecl_filename_char *p;
    i = ecl_length(user);
    /* This ensures that our string has the right length
       and it is terminated with a '\0' */
    user = ecl_encode_filename(user, ECL_NIL);
    p = ecl_filename_self(user);
    if (i > 0 && *p == '~') {
      p++;
      i--;
    }
    if (i == 0)
      return ecl_homedir_pathname(ECL_NIL);
#ifdef HAVE_PWD_H
    pwent = getpwnam(p);
    if (pwent == NULL)
      FEerror("Unknown user ~S.", 1, p);
    namestring = ecl_make_simple_filename(pwent->pw_dir,-1);
#endif
    FEerror("Unknown user ~S.", 1, p);
  } else if ((h = ecl_getenv(ecl_fstr("HOME")))) {
    namestring = ecl_make_simple_filename(h,-1);
#if defined(ECL_MS_WINDOWS_HOST)
  } else if ((h = ecl_getenv(ecl_fstr("HOMEPATH"))) && (d = ecl_getenv(ecl_fstr("HOMEDRIVE")))) {
    namestring = ecl_concatenate_filename(ecl_make_constant_filename(d,-1),
                                          ecl_make_constant_filename(h,-1));
#endif
  } else {
    namestring = @"/";
  }
  if (ecl_filename_self(namestring)[0] == '~') {
    FEerror("Not a valid home pathname ~S", 1, namestring);
  }
  i = namestring->base_string.fillp;
  if (!IS_DIR_SEPARATOR(ecl_filename_self(namestring)[i-1]))
    namestring = ecl_concatenate_filename(namestring,
                                          si_coerce_to_base_string(ECL_CODE_CHAR(DIR_SEPARATOR)));
  namestring = ecl_decode_filename(namestring, ECL_NIL);
  return cl_parse_namestring(3, namestring, ECL_NIL, ECL_NIL);
}

@(defun user_homedir_pathname (&optional host)
  @
  /* Ignore optional host argument. */
  @(return ecl_homedir_pathname(ECL_NIL));
  @)

static bool
string_match(const ecl_filename_char *s, cl_object pattern)
{
  if (pattern == ECL_NIL || pattern == @':wild') {
    return 1;
  } else {
    cl_object string_decoded = ecl_decode_filename(ecl_make_constant_filename(s,-1), ECL_NIL);
    return ecl_string_match(string_decoded, 0, string_decoded->base_string.fillp,
                            pattern, 0, ecl_length(pattern));
  }
}

static inline cl_object
parse_directory_entry(const ecl_filename_char *text, cl_object text_mask, cl_object prefix, cl_object pathname_mask, int flags)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object component, component_string, component_path, kind;
  if (text[0] == '.' &&
      (text[1] == '\0' ||
       (text[1] == '.' && text[2] == '\0')))
    return ECL_NIL;
  if (!string_match(text, text_mask))
    return ECL_NIL;
  component = ecl_make_constant_filename(text,-1);
  component = ecl_concatenate_filename(prefix, component);
  component_string = ecl_decode_filename(component, ECL_NIL);
  component_path = cl_pathname(component_string);
  if (!Null(pathname_mask)) {
    if (Null(cl_pathname_match_p(component_string, pathname_mask)))
      return ECL_NIL;
  }
  component_path = file_truename(component_path, component, flags);
  kind = ecl_nth_value(the_env, 1);
  return CONS(component_path, kind);
}

/*
 * list_current_directory() lists the files and directories which are contained
 * in the current working directory (as given by current_dir()). If ONLY_DIR is
 * true, the list is made of only the directories -- a propert which is checked
 * by following the symlinks.
 */
static cl_object
list_directory(cl_object base_dir, cl_object text_mask, cl_object pathname_mask,
               int flags)
{
  cl_object out = ECL_NIL;
  cl_object prefix = ecl_namestring(base_dir, ECL_NAMESTRING_FORCE_BASE_STRING);
  cl_object entry;

#ifdef ECL_MS_WINDOWS_HOST
  ecl_WIN32_FIND_DATA fd;
  HANDLE hFind = NULL;
  BOOL found = FALSE;

  ecl_disable_interrupts();
  for (;;) {
    if (hFind == NULL) {
      cl_object aux = @".\\*";
      cl_object mask = ecl_concatenate_filename(prefix, aux);
      hFind = ecl_FindFirstFile(ecl_filename_self(mask), &fd);
      if (hFind == INVALID_HANDLE_VALUE) {
        out = ECL_NIL;
        goto OUTPUT;
      }
      found = TRUE;
    } else {
      found = ecl_FindNextFile(hFind, &fd);
    }

    if (!found)
      break;

    if (!Null(entry = parse_directory_entry(fd.cFileName, text_mask, prefix, pathname_mask, flags)))
      out = CONS(entry, out);
  }
  FindClose(hFind);
#elif defined(HAVE_DIRENT_H)
  DIR *dir;
  struct dirent *d_entry;

  ecl_disable_interrupts();
  dir = opendir(ecl_filename_self(prefix));
  if (dir == NULL) {
    out = ECL_NIL;
    goto OUTPUT;
  }

  while ((d_entry = readdir(dir))) {
    if (!Null(entry = parse_directory_entry(d_entry->d_name, text_mask, prefix, pathname_mask, flags)))
      out = CONS(entry, out);
  }
  closedir(dir);
#else /* sys/dir.h as in SYSV */
  FILE *fp;
  char iobuffer[BUFSIZ];
  DIRECTORY dir;

  ecl_disable_interrupts();
  fp = ecl_fopen(ecl_filename_self(prefix), OPEN_R);
  if (fp == NULL) {
    out = ECL_NIL;
    goto OUTPUT;
  }
  setbuf(fp, iobuffer);
  for (;;) {
    if (fread(&dir, sizeof(DIRECTORY), 1, fp) <= 0)
      break;
    if (dir.d_ino == 0)
      continue;

    if (!Null(entry = parse_directory_entry(dir.d_name, text_mask, prefix, pathname_mask, flags)))
      out = CONS(entry, out);
  }
  fclose(fp);
#endif

 OUTPUT:
  ecl_enable_interrupts();
  return cl_nreverse(out);
}

/*
 * dir_files() lists all files which are contained in the current directory and
 * which match the masks in PATHNAME. This routine is essentially a wrapper for
 * list_current_directory(), which transforms the list of strings into a list
 * of pathnames. BASEDIR is the truename of the current directory and it is
 * used to build these pathnames.
 */
static cl_object
dir_files(cl_object base_dir, cl_object pathname, int flags)
{
  cl_object all_files, output = ECL_NIL;
  cl_object mask;
  cl_object name = pathname->pathname.name;
  cl_object type = pathname->pathname.type;
  if (name == ECL_NIL && type == ECL_NIL) {
    return cl_list(1, base_dir);
  }
  mask = ecl_make_pathname(ECL_NIL, ECL_NIL, ECL_NIL,
                           name, type, pathname->pathname.version,
                           @':local');
  for (all_files = list_directory(base_dir, ECL_NIL, mask, flags);
       !Null(all_files);
       all_files = ECL_CONS_CDR(all_files))
    {
      cl_object record = ECL_CONS_CAR(all_files);
      cl_object new = ECL_CONS_CAR(record);
      cl_object kind = ECL_CONS_CDR(record);
      if (kind != @':directory') {
        output = CONS(new, output);
      }
    }
  return output;
}

/*
 * dir_recursive() performs the dirty job of DIRECTORY. The routine moves
 * through the filesystem looking for files and directories which match
 * the masks in the arguments PATHNAME and DIRECTORY, collecting them in a
 * list.
 */
static cl_object
dir_recursive(cl_object base_dir, cl_object directory, cl_object filemask, int flags)
{
  cl_object item, output = ECL_NIL;
 AGAIN:
  /* There are several possibilities here:
   *
   * 1) The list of subdirectories DIRECTORY is empty, and only PATHNAME
   * remains to be inspected. If there is no file name or type, then
   * we simply output the truename of the current directory. Otherwise
   * we have to find a file which corresponds to the description.
   */
  if (directory == ECL_NIL) {
    return ecl_nconc(dir_files(base_dir, filemask, flags), output);
  }
  /*
   * 2) We have not yet exhausted the DIRECTORY component of the
   * pathname. We have to enter some subdirectory, determined by
   * CAR(DIRECTORY) and scan it.
   */
  item = ECL_CONS_CAR(directory);

  if (item == @':wild' || ecl_wild_string_p(item)) {
    /*
     * 2.1) If CAR(DIRECTORY) is a string or :WILD, we have to
     * enter & scan all subdirectories in our curent directory.
     */
    cl_object next_dir = list_directory(base_dir, item, ECL_NIL, flags);
    for (; !Null(next_dir); next_dir = ECL_CONS_CDR(next_dir)) {
      cl_object record = ECL_CONS_CAR(next_dir);
      cl_object component = ECL_CONS_CAR(record);
      cl_object kind = ECL_CONS_CDR(record);
      if (kind != @':directory')
        continue;
      item = dir_recursive(cl_pathname(component),
                           ECL_CONS_CDR(directory),
                           filemask, flags);
      output = ecl_nconc(item, output);
    }
  } else if (item == @':wild-inferiors') {
    /*
     * 2.2) If CAR(DIRECTORY) is :WILD-INFERIORS, we have to do
     * scan all subdirectories from _all_ levels, looking for a
     * tree that matches the remaining part of DIRECTORY.
     */
    cl_object next_dir = list_directory(base_dir, ECL_NIL, ECL_NIL, flags);
    for (; !Null(next_dir); next_dir = ECL_CONS_CDR(next_dir)) {
      cl_object record = ECL_CONS_CAR(next_dir);
      cl_object component = ECL_CONS_CAR(record);
      cl_object kind = ECL_CONS_CDR(record);
      if (kind != @':directory')
        continue;
      item = dir_recursive(cl_pathname(component),
                           directory, filemask, flags);
      output = ecl_nconc(item, output);
    }
    directory = ECL_CONS_CDR(directory);
    goto AGAIN;
  } else { /* :ABSOLUTE, :RELATIVE, :UP, component without wildcards */
    /*
     * 2.2) If CAR(DIRECTORY) is :ABSOLUTE, :RELATIVE or :UP we update
     * the directory to reflect the root, the current or the parent one.
     */
    base_dir = enter_directory(base_dir, item, 1);
    /*
     * If enter_directory() fails, we simply ignore this path. This is
     * what other implementations do and is consistent with the behavior
     * for the file part.
     */
    if (Null(base_dir))
      return ECL_NIL;
    directory = ECL_CONS_CDR(directory);
    goto AGAIN;
  }
  return output;
}

@(defun directory (mask &key (resolve_symlinks ECL_T) &allow_other_keys)
  cl_object base_dir;
  cl_object output;
  @
  mask = si_coerce_to_file_pathname(mask);
  mask = make_absolute_pathname(mask);
  base_dir = make_base_pathname(mask);
  output = dir_recursive(base_dir, mask->pathname.directory, mask,
                         Null(resolve_symlinks)? 0 : FOLLOW_SYMLINKS);
  @(return output);
  @)

@(defun ext::getcwd (&optional (change_d_p_d ECL_NIL))
  cl_object output;
  @
  output = cl_parse_namestring(3, ecl_decode_filename(current_dir(), ECL_NIL), ECL_NIL, ECL_NIL);
  if (!Null(change_d_p_d)) {
    ECL_SETQ(the_env, @'*default-pathname-defaults*', output);
  }
  @(return output);
  @)

cl_object
si_get_library_pathname(void)
{
  cl_object s = cl_core.library_pathname;
  if (!Null(s)) {
    goto OUTPUT_UNCHANGED;
  } else {
    ecl_filename_char *v = ecl_getenv(ecl_fstr("ECLDIR"));
    if (v) {
      s = ecl_make_constant_filename(v,-1);
      goto OUTPUT;
    }
  }
#if defined(ECL_MS_WINDOWS_HOST)
  {
    ecl_filename_char *buffer;
    HMODULE hnd;
    cl_index len, ep;
    s = ecl_alloc_adjustable_filename(cl_core.path_max);
    buffer = ecl_filename_self(s);
    ecl_disable_interrupts();
    hnd = GetModuleHandle("ecl.dll");
    len = ecl_GetModuleFileName(hnd, buffer, cl_core.path_max-1);
    ecl_enable_interrupts();
    if (len == 0) {
      FEerror("GetModuleFileName failed (last error = ~S)",
              1, ecl_make_fixnum(GetLastError()));
    }
    /* GetModuleFileName returns a file name. We have to strip
     * the directory component. */
    for (; len > 0 && buffer[len-1] != '\\'; len--);
    buffer[len] = '\0';
    s->base_string.fillp = len;
  }
#else
  s = ecl_make_constant_base_string(ECLDIR "/",-1);
#endif
 OUTPUT:
  {
    if (file_kind(ecl_filename_self(s), TRUE) == ECL_NIL) {
      s = current_dir();
    }
  }
  cl_core.library_pathname = ecl_decode_filename(s, ECL_NIL);
 OUTPUT_UNCHANGED:
  @(return cl_core.library_pathname);
}

@(defun ext::chdir (directory &optional (change_d_p_d ECL_T))
  cl_object previous = si_getcwd(0);
  cl_object namestring;
  @
  /* This will fail if the new directory does not exist */
  directory = cl_truename(directory);
  if (directory->pathname.name != ECL_NIL ||
      directory->pathname.type != ECL_NIL)
    FEerror("~A is not a directory pathname.", 1, directory);
  namestring = ecl_namestring(directory,
                              ECL_NAMESTRING_TRUNCATE_IF_ERROR |
                              ECL_NAMESTRING_FORCE_BASE_STRING);
  if (safe_chdir(ecl_filename_self(namestring), ECL_NIL) < 0) {
    cl_object c_error = _ecl_strerror(errno);
    const char *msg = "Can't change the current directory to ~A."
      "~%C library error: ~S";
    si_signal_simple_error
      (6, @'file-error', /* condition */
       ECL_T, /* continuable */
       /* format */
       ecl_make_constant_base_string(msg,strlen(msg)),
       cl_list(2, directory, c_error), /* format args */
       @':pathname', /* file-error options */
       directory);
  } else if (change_d_p_d != ECL_NIL) {
    ECL_SETQ(the_env, @'*default-pathname-defaults*', directory);
  }
  @(return previous);
  @)

cl_object
si_mkdir(cl_object directory, cl_object mode)
{
  int modeint, ok;
  cl_object filename;
  {
    /* Ensure a clean string, without trailing slashes,
     * and null terminated */
    cl_index last = ecl_length(directory);
    if (last > 1) {
      ecl_character c = ecl_char(directory, last-1);
      if (IS_DIR_SEPARATOR(c))
        last--;
    }
    filename = ecl_encode_filename(directory, ecl_make_fixnum(last));
  }

  if (ecl_unlikely(!ECL_FIXNUMP(mode) ||
                   ecl_fixnum_minusp(mode) ||
                   ecl_fixnum_greater(mode, ecl_make_fixnum(0777)))) {
    FEwrong_type_nth_arg(@[si::mkdir], 2, mode,
                         ecl_make_integer_type(ecl_make_fixnum(0),
                                               ecl_make_fixnum(0777)));
  }
  modeint = ecl_fixnum(mode);
  ecl_disable_interrupts();
#if defined(ECL_MS_WINDOWS_HOST)
  ok = ecl_mkdir(ecl_filename_self(filename));
#else
  ok = ecl_mkdir(ecl_filename_self(filename), modeint);
#endif
  ecl_enable_interrupts();

  if (ecl_unlikely(ok < 0)) {
    cl_object c_error = _ecl_strerror(errno);
    const char *msg = "Could not create directory ~S"
      "~%C library error: ~S";
    si_signal_simple_error
      (6, @'file-error', /* condition */
       ECL_T, /* continuable */
       /* format */
       ecl_make_constant_base_string(msg,strlen(msg)),
       cl_list(2, filename, c_error), /* format args */
       @':pathname', /* file-error options */
       filename);
  }
  @(return filename);
}

cl_object
si_mkstemp(cl_object template)
{
  cl_object output;
  cl_index l;
  int fd;

#if defined(ECL_MS_WINDOWS_HOST)
  cl_object phys, dir, file;
  ecl_filename_char strTempDir[MAX_PATH];
  ecl_filename_char strTempFileName[MAX_PATH];
  ecl_filename_char *s;
  int ok;

  phys = cl_translate_logical_pathname(1, template);
  dir = cl_make_pathname(8,
                         @':type', ECL_NIL,
                         @':name', ECL_NIL,
                         @':version', ECL_NIL,
                         @':defaults', phys);
  dir = si_coerce_to_filename(dir);
  file = cl_file_namestring(phys);
        
  l = dir->base_string.fillp;
  ecl_fstrcpy(strTempDir, ecl_filename_self(dir));
  strTempDir[l] = 0;
  for (s = strTempDir; *s; s++)
    if (*s == '/')
      *s = '\\';

  ecl_disable_interrupts();
  ok = ecl_GetTempFileName(strTempDir, ecl_filename_self(file), 0,
                           strTempFileName);
  ecl_enable_interrupts();
  if (!ok) {
    output = ECL_NIL;
  } else {
    output = ecl_make_simple_filename(strTempFileName,-1);
  }
#else
  template = si_coerce_to_filename(template);
  l = template->base_string.fillp;
  output = ecl_alloc_simple_filename(l + 6);
  ecl_fstrcat(ecl_fstrcpy(ecl_filename_self(output), ecl_filename_self(template)),
              ecl_fstr("XXXXXX"));

  ecl_disable_interrupts();
# ifdef HAVE_MKSTEMP
  fd = mkstemp(ecl_filename_self(output));
# else
  if (mktemp(ecl_filename_self(output))) {
    fd = ecl_open(ecl_filename_self(output), O_CREAT|O_TRUNC, 0666);
  } else {
    fd = -1;
  }
# endif
  ecl_enable_interrupts();

  if (fd < 0) {
    output = ECL_NIL;
  } else {
    close(fd);
  }
#endif
  @(return (Null(output)? output : cl_truename(ecl_decode_filename(output, ECL_NIL))));
}

cl_object
si_rmdir(cl_object directory)
{
  return cl_delete_file(cl_make_pathname(6, @':name', ECL_NIL,
                                         @':type', ECL_NIL,
                                         @':defaults', directory));
}

cl_object
si_copy_file(cl_object orig, cl_object dest)
{
  FILE *in, *out;
  int ok = 0;
  orig = si_coerce_to_filename(orig);
  dest = si_coerce_to_filename(dest);
  ecl_disable_interrupts();
  in = ecl_fopen(ecl_filename_self(orig), OPEN_R);
  if (in) {
    out = ecl_fopen(ecl_filename_self(dest), OPEN_W);
    if (out) {
      unsigned char *buffer = ecl_alloc_atomic(1024);
      cl_index size;
      do {
        size = fread(buffer, 1, 1024, in);
        fwrite(buffer, 1, size, out);
      } while (size == 1024);
      ok = 1;
      fclose(out);
    }
    fclose(in);
  }
  ecl_enable_interrupts();
  @(return (ok? ECL_T : ECL_NIL));
}

cl_object
si_chmod(cl_object file, cl_object mode)
{
  mode_t code = ecl_to_uint32_t(mode);
  cl_object filename = si_coerce_to_filename(file);
  unlikely_if (ecl_chmod(ecl_filename_self(filename), code)) {
    cl_object c_error = _ecl_strerror(errno);
    const char *msg = "Unable to change mode of file ~S to value ~O"
      "~%C library error: ~S";
    si_signal_simple_error
      (6, @'file-error', /* condition */
       ECL_T, /* continuable */
       /* format */
       ecl_make_constant_base_string(msg,strlen(msg)),
       cl_list(3, file, mode, c_error), /* format args */
       @':pathname', /* file-error options */
       file);
  }
  @(return);
}
