
t = int(input())

def minthick(arr):

    s = 0
    for x in arr:
        s += x
        

for _ in range(t):
    n = int(input())
    a = [int(x) for x in input().split(" ")]

    print(minthick(a))