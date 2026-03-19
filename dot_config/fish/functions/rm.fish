function rm
    if status is-interactive
        echo "Use trash instead (alias rt)."
        return 1
    end
    command rm $argv
end
