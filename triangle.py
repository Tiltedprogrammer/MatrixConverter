import sys

header = "%%MatrixMarket matrix coordinate integer general\n"

if __name__ == "__main__":
    if len(sys.argv) == 3:
        filename = sys.argv[2]
        n = int(sys.argv[1])

        size = int((n + 1)* n / 2)
        with open(filename, "w") as f:
            f.write(header)
            f.write(f"{n}   {n}    {size}\n")
        
        
            #for each row
            for i in range(1,n+1):
                for j in range(1,i + 1):
                  f.write(f"    {i}    {j}    {1}\n")

    else:
        print("Matrix size and filename are expected: triangle.py `n` `filename`")