import sys
import numpy as np
import argparse

header = "%%MatrixMarket matrix coordinate integer general\n"

def generate_lower_triangle(n):
    
    size = int((n + 1) * n / 2)
    
    matrix = np.empty((size, 3), dtype='int')
    
    row = 0
    for i in range(1,n+1):
                for j in range(1,i + 1):
                    matrix[row] = [i, j, 1]
                    row = row + 1
    return matrix


def generate_diagonal(n):
    
    size = n

    matrix = np.empty((size, 3), dtype='int')
    
    row = 0
    
    for i in range(1, n + 1):
        matrix[row] = [i, i, 1]
        row = row + 1
    
    return matrix 


parser = argparse.ArgumentParser()

parser.add_argument("-t", "--triangle", type=int,
                    help="Generate lower triangle matrix of the given size")


parser.add_argument("-d", "--diagonal", type=int,
                    help="Generate diagonal matrix of the given size")

parser.add_argument("filename", type=str, help="filename to store the matrix")

args = parser.parse_args()

def write_matrix_to_file(matrix, dim, filename):
    
    with open(filename, "w") as f:
            
            f.write(header)
            f.write(f"{dim}   {dim}    {matrix.shape[0]}\n")
        
            for line in matrix:
                f.write(' '.join(map(str,line)) + '\n')


# if __name__ == "__main__":

#     if len(sys.argv) == 3:
#         filename = sys.argv[2]
#         n = int(sys.argv[1])

#         size = int((n + 1)* n / 2)
#         with open(filename, "w") as f:
#             f.write(header)
#             f.write(f"{n}   {n}    {size}\n")
        
        
#             #for each row
#             for i in range(1,n+1):
#                 for j in range(1,i + 1):
#                   f.write(f"    {i}    {j}    {1}\n")

#     else:
#         print("Matrix size and filename are expected: triangle.py `n` `filename`")

if __name__ == "__main__":
    
    if args.triangle:
        write_matrix_to_file(generate_lower_triangle(args.triangle), args.triangle, args.filename)
    elif args.diagonal:
        write_matrix_to_file(generate_diagonal(args.diagonal), args.diagonal, args.filename)
    else:
        pass