#!/bin/sh

OS_TYPES="unx w32 os2"
PKG_SUFFIXES="crf fre pkd"
SCM_SUFFIXES="bin ext com bci"
SCHEME_FILES="mit-etc mit-syntax mit-suspend primitive rendezvous mit-time"
SCHEME_FILES="${FILES} rendezvous-syntax channel mailbox placeholder semaphore"
GENERATED_FILES="mit-syntax.scm mit-suspend.scm"

begin_delete ()
{
  printf 'Delete'
}

delete ()
{
  local file
  file="${1}"
  printf ' %s' "${file}"
  rm -f "${file}"
}

end_delete ()
{
  printf '\n'
}

for os_type in ${OS_TYPES}; do
  begin_delete
  for pkg_suffix in ${PKG_SUFFIXES}; do
    delete "mit-rendezvous-${os_type}.${pkg_suffix}"
  done
  end_delete
done

for file in ${SCHEME_FILES}; do
  begin_delete 
  for scm_suffix in ${SCM_SUFFIXES}; do
    delete "${file}.${scm_suffix}"
  done
  end_delete
done

begin_delete
for os_type in ${OS_TYPES}; do
  delete "mit-runtime-${os_type}.pkd"
done
end_delete

begin_delete
for generated_file in ${GENERATED_FILES}; do
  delete "${generated_file}"
done
end_delete
