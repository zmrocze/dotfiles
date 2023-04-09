
t = int(input())

def minthick(arr):

    s = 0
    for i in range(len(arr)):
        s += arr[i]

        mint = i+1
        j = i+1

        while j < len(arr):
            s1 = arr[j]    
            while s1 < 


for _ in range(t):
    n = int(input())
    a = [int(x) for x in input().split(" ")]

    print(minthick(a))