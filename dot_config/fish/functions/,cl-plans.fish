function ,cl-plans
    set port (test (count $argv) -gt 0; and echo $argv[1]; or echo 9999)
    algernon --cache=off --theme=dark -t ~/.claude/plans :$port
end
