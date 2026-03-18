function fish_greeting
  set -l greetings (string split \n -- (string trim -- "
hello world
hack the planet
with great power comes great responsibility
'you miss 100% of the shots you don’t take. - wayne gretzky' - michael scott
  "))

  set chosen_msg $greetings[(random 1 (count $greetings))]

  printf '%s\n' (set_color F90)$chosen_msg
end
