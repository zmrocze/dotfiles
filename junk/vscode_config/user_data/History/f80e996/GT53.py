
t = int(input())

def minthick(arr):

    s = 0
    Mmint = float("inf")
    for i in range(len(arr)):
        s += arr[i]

        mint = i+1
        j = i+1
        prev_j = i
        while j < len(arr):
            s1 = arr[j]    
            while s1 < s:
                j += 1
                s1 += arr[j]
            if s1 == s:
                mint = max(mint, j-prev_j)
                prev_j = j
                j += 1
                continue
            else:
                break
        Mmint = min(mint, Mmint)
    
    return Mmint

for _ in range(t):
    n = int(input())
    a = [int(x) for x in input().split(" ")]

    print(minthick(a))