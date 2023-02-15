# © Pierre Lescanne (ENS de Lyon)
# =========================
#   Non recursive,
# with an explicit stack
# =========================
from collections import deque
import random
import PreMotzkin

def oracle(i,rand):
  return (i <= 10004 and PreMotzkin.ratioM[i] <= rand) or 0.6666666666791656 <= rand 

# given n, buils a stack of Boolean True and False
# if the test is True then one decrement by 1
# if the test is False then one decrement by 2
def builds(n):
  print("WARNING: This is not guaranteed to be uniformy distributed!")
  i = n
  stack = deque()
  while i > 1:
    rand = random.random()
    if oracle(i,rand):
      stack.append(True)
      i = i - 1
    else:
      stack.append(False)
      i = i - 2
  if i == 1:
    stack.append(False)
  else:
    stack.append(True)
  return stack    

def rmt(n): # a faire
  stack = builds(n)
  b = stack.pop()
  if b:
    v = [1,0,2]
    i = 0
  else:
    v = [1,3,0,2,4]
    i = 1
 
  while not(not(stack)): # while stack is not empty
    b = stack.pop()
    if b:
      i = i + 1 
      k = random.randint(0,2*i)
      if (k%2==1)or(v[k]%2==1)or(v[k-1]%2==1):
        v.append(v[k])
        v.append(2*i+2)
        v[k] = 2*i+1
      else:
        v.append(v[k-1])
        v.append(2*i+2)
        v[k-1] = 2*i+1
    else:
      i = i + 2
      r = random.randint(0,3*i - 6)
      k = int(r // 3)
      if r % 3  < 2:
        v.append(2*i)
        v.append(2*i+2)
        v.append(v[2*k+1])
        v.append(v[2*k+2])
        v[2*k+1] = 2*i-1
        v[2*k+2] = 2*i+1
      else:
        v.append(v[2*k+1])
        v.append(v[2*k+2])
        v.append(2*i)
        v.append(2*i+2)
        v[2*k+1] = 2*i-1
        v[2*k+2] = 2*i+1
  return v

# ===========
#   TEST
# ===========

def aTree(size,seed):
  print("--------------------------------------")
  print("size = ", size, "seed = ", seed)
  random.seed(seed)
  l = rmt(size)
  return list(zip(range(0,len(l)),l))

