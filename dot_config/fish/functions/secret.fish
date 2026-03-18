function secret
  age -d "$AGE_SECRETS" | grep "^$argv[1]=" | cut -d= -f2-
end
