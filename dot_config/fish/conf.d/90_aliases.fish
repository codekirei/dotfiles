function g --description 'git'
  git $argv
end

function e --description 'edit with $EDITOR'
  $EDITOR $argv
end
