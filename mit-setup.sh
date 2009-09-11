#!/bin/sh

set -e

usage ()
{
  printf 'Usage: %s [mutex|interrupt] /path/to/mit-scheme/src\n' "${0}"
  printf '  /path/to/mit-scheme/src must contain a build of MIT Scheme.\n'
  printf '  Current directory must be the rendezvous source directory.\n'
  exit 1
}

if [ ! -f mit-rendezvous.pkg ]; then
  usage
fi

if [ $# -ne 2 ]; then
  usage
fi

TYPE="${1}"
MIT_SRC="${2}"
OS_TYPES="unx w32 os2"

case "${TYPE}" in
  mutex|interrupt)
    ;;
  *)
    usage
    ;;
esac

for os_type in ${OS_TYPES}; do
  if [ ! -f "${MIT_SRC}/runtime/runtime-${os_type}.pkd" ]; then
    usage
  fi
done

link ()
{
  local file_pathname link_pathname
  file_pathname="${1}"
  link_pathname="${2}"
  printf 'Link %s -> %s\n' "${link_pathname}" "${file_pathname}"
  rm -f "${link_pathname}"
  ln -s "${file_pathname}" "${link_pathname}"
}

link "mit-${TYPE}-syntax.scm" mit-syntax.scm
link "mit-${TYPE}-suspend.scm" mit-suspend.scm
for os_type in ${OS_TYPES}; do
  link "${MIT_SRC}/runtime/runtime-${os_type}.pkd" "mit-runtime-${os_type}.pkd"
done
