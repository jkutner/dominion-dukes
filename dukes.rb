def score(dukes, duchies)
  duchies*3 + dukes*duchies
end

def play(dukes, duchies)
  dukes_in_deck = 0
  duchies_in_deck = 0
  
  while (dukes > 0 and duchies > 0)
    if score(dukes_in_deck+1,  duchies_in_deck) >= score(dukes_in_deck, duchies_in_deck+1)
      dukes_in_deck += 1
      dukes -= 1
      puts "duke ->   #{score(dukes_in_deck, duchies_in_deck)}"
    else
      duchies_in_deck += 1
      duchies -= 1
      puts "duchy ->  #{score(dukes_in_deck, duchies_in_deck)}"
    end
  end
end

play(8,8)