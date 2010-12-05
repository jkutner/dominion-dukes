def score(dukes, duchies)
  duchies*3 + dukes*duchies
end

def play(dukes, duchies)
  dukes_in_hand = 0
  duchies_in_hand = 0
  
  while (dukes > 0 and duchies > 0)
    if score(dukes_in_hand+1,  duchies_in_hand) >= score(dukes_in_hand, duchies_in_hand+1)
      dukes_in_hand += 1
      dukes -= 1
      puts "duke ->   #{score(dukes_in_hand, duchies_in_hand)}"
    else
      duchies_in_hand += 1
      duchies -= 1
      puts "duchy ->  #{score(dukes_in_hand, duchies_in_hand)}"
    end
  end
end

play(8,8)