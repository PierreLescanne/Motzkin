import random

def rst(n):
  v = [(0,False)]
  nb_fails = 0
  for i in range(1,n+1):
    while True:
      rand = random.random()
      x = int (rand * (6*i-4))
      k = int(x // 3)
      xmod3 = x % 3
      if xmod3 == 0: # L1
        v.append((2*i,False))
        v.append(v[k])
        v[k] = ((2*i-1,False))
        break
      elif xmod3 == 1: # L2
        if v[k][0] % 2 == 1: # fst (v[k]) is odd
           v.append((2*i,False))
           v.append((v[k][0],True))
           v[k] = ((2*i-1,False))
           break
        else:
          if k % 2 == 1: # k is odd
             if v[k+1][1]: # the other leaf is white
               nb_fails = nb_fails + 1
             else:   # the other leaf is black
               v.append(v[k])
               v.append((2*i,False))
               v[k] = v[k+1]
               v[k+1] = ((2*i-1,True))
               break
          else:
            v.append(v[k])
            v.append((2*i,False))
            v[k] = (2*i-1,True)
            break
      elif xmod3 == 2 : # L3
        v.append(v[k])
        v.append((2*i,False))
        v[k] = ((2*i-1,False))
        break
      else:
        print("impossible")
        break
  print("--------------------------------------")
  print("The size of the Schroeder tree is ", n)
  print("The program fails ", nb_fails, " times")
  print("The ratio number of fails over size is ", float(nb_fails) / float(n))
  return v
