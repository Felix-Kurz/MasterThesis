#!/bin/tcsh
# Freeze an R/RStudio process before it can consume the whole XXL capsule.
# Usage: tcsh watch_capsule_memory.csh PID [stop_at_GiB]
# Default stop threshold is 460 GiB, leaving roughly 52 GB below 512 GB.

if ($#argv < 1 || $#argv > 2) then
  echo "Usage: tcsh watch_capsule_memory.csh PID [stop_at_GiB]"
  exit 1
endif

set watched_pid = "$argv[1]"
set stop_gib = 460
if ($#argv == 2) set stop_gib = "$argv[2]"

if (! -d "/proc/$watched_pid") then
  echo "PID $watched_pid does not exist."
  exit 1
endif

@ stop_kib = $stop_gib * 1048576
echo "Watching PID $watched_pid; SIGSTOP at ${stop_gib} GiB RSS."
echo "To terminate after SIGSTOP: kill -TERM $watched_pid; kill -CONT $watched_pid"
echo "Or resumed with: kill -CONT $watched_pid"

while (-d "/proc/$watched_pid")
  set rss_kib = `awk '/^VmRSS:/ {print $2}' /proc/$watched_pid/status`
  if ("$rss_kib" != "") then
    @ rss_gib = $rss_kib / 1048576
    echo "`date '+%Y-%m-%d %H:%M:%S'`  PID=$watched_pid  RSS=${rss_gib}GiB"

    if ($rss_kib >= $stop_kib) then
      echo "Threshold reached. Freezing PID $watched_pid with SIGSTOP."
      kill -STOP $watched_pid
      exit 2
    endif
  endif
  sleep 2
end

echo "PID $watched_pid exited."
