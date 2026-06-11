# [i]nject [s]ecret [e]nv

function ,ise
  set -l val (secret $argv[1])
  set -gx $argv[2] $val
end
