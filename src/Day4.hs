module Day4 where

input = [123257..647015]

check password = fact password 10 False
  where
    fact p n adj =
      let (p', n') = p `quotRem` 10 in
        if p' == 0 && n' == 0 then adj else
          if n < n'           then False else
            if n  ==  n'      then fact p' n' True else fact p' n' adj

passwords = filter check input

day4Part1 = length passwords

newCheck password = newFact password 10 11 False
  where
    newFact p n nadj adj =
      let (p', n') = p `quotRem` 10 in
        if p' == 0 && n' == 0  then (if nadj == 1 then True else adj) else
          if n < n'            then False else
            if n' == n         then newFact p' n' (nadj+1) adj else newFact p' n' 0 (if nadj == 1 then True else adj)

newPasswords = filter newCheck input

day4Part2 = length newPasswords
