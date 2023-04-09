
t = int(input())

def minthick(arr):

    s = 0
    Mmint = float("inf")
    for i in range(len(arr)):
        # print(f"i: {i}")
        s += arr[i]

        mint = i+1
        j = i+1
        prev_j = i
        while j < len(arr):
            s1 = arr[j]  
            while s1 < s and j+1 < len(arr):
                j += 1
                s1 += arr[j]
            if s1 == s:
                mint = max(mint, j-prev_j)
                # print(j-prev_j)
                prev_j = j
                j += 1
                if j >= len(arr):
                    Mmint = min(mint, Mmint)
                continue
            else:
                break
    
    return Mmint

for _ in range(t):
    n = int(input())
    a = [int(x) for x in input().split(" ")]

    print(minthick(a))