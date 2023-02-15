# © Pierre Lescanne (ENS de Lyon)
import random

def rbt(n):
  v = [0]
  for i in range(1,n+1):
      rand = random.random()
      x = int(rand * (4*i-3))
      k = int(x // 2)
      if (k % 2) == 0:
          v.append(v[k])
          v.append(2*i)
          v[k] = 2*i-1
      else:
          v.append(2*i)
          v.append(v[k])
          v[k] = 2*i-1
  print("--------------------------------------")
  print("The size of the binary tree is ", n)
  return v
