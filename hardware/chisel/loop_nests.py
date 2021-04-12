# Modified by contributors from Intel Labs

import numpy as np

# TensorGEMM is 1 x c times c x c = 1 x c
# The C scratchpad size is b x b 
# q is the number of bytes read per cycle

(b, c, q) = (64, 16, 8)
#(b, c, q) = (4, 2, 1)

(M, K, N) = (1024, 1024, 1024)
#(M, K, N) = (b, c, b)

As = np.zeros( (2, b, c), dtype=np.int8)
Bs = np.zeros( (2, b//c, c*c), dtype=np.int8)
Cs = np.zeros( (2, b*b//c, c), dtype=np.int32)

A = np.random.randint( -128, 127, size=(M, K), dtype=np.int8)
B = np.random.randint( -128, 127, size=(K, N), dtype=np.int8)
C = np.zeros( (M, N), dtype=np.int32)

Cref = (A.astype(np.int32).dot(B)).astype(np.int8)

a_loads = 0
b_loads = 0
c_stores = 0

def MVM( a, b, o):
    o += a.astype(np.int32).dot(b.reshape( (c,c)))

def loadA( I, k, parity):
    global a_loads
    for i in range( 0, b, 1):
        for ki in range( 0, c, q):
            a_loads += q
            As[parity,i,ki:ki+q] = \
              A[I+i,k+ki:k+ki+q]

def loadB( J, k, parity):
    global b_loads
    for jo in range( 0, b, c):
        for ki in range( 0, c, 1):
            for ji in range( 0, c, q):
                b_loads += q
                Bs[parity,jo//c,ki*c+ji:ki*c+ji+q] = \
                  B[k+ki,J+jo+ji:J+jo+ji+q]

def resetC( cparity):
    for j in range( 0, b//c, 1):
        for i in range( 0, b, 1):
            Cs[cparity,i*(b//c)+j] = 0
    

def compute( parity, cparity):
    for j in range( 0, b//c, 1):
        for i in range( 0, b, 1):
            MVM( As[parity,i], Bs[parity,j], Cs[cparity,i*(b//c)+j])

def storeC( I, J, cparity):
    global c_stores
    for j in range( 0, b//c, 1):
        for i in range( 0, b, 1):
            for ji in range( 0, c, q):
                c_stores += q
                C[I+i,J+j*c+ji:J+j*c+ji+q] = \
                  Cs[cparity,i*(b//c)+j,ji:ji+q].astype(np.int8)

def main():
    assert M % b == 0
    assert N % b == 0
    assert K % c == 0
    assert c % q == 0

    def p( s):
        return s[0]*s[1]*s[2]
    print( f"A scratchpad size is: {As.shape} bytes ({p(As.shape)}B)")
    print( f"B scratchpad size is: {Bs.shape} bytes ({p(Bs.shape)}B)")
    print( f"C scratchpad size is: {Cs.shape} words ({4*p(Cs.shape)}B)")

    parity = 0
    cparity = 0
    for I in range( 0, M, b):
        for J in range( 0, N, b):
            resetC( cparity)
            for k in range( 0, K, c):
                loadA( I, k, parity)
                loadB( J, k, parity)
                compute( parity, cparity)
                parity = (parity + 1) % 2
            storeC( I, J, cparity)
            cparity = (cparity + 1) % 2

    print(C)
    print(Cref)
    print(np.isclose(C,Cref))
    assert np.isclose(C,Cref).all()

    print( f"Estimated reads from A {M*K*(N//b)}")
    print( f"Estimated reads from B {(M//b)*K*N}")
    print( f"Estimated stores to C {M*N}")

    print( f"Simulated reads from A {a_loads}")
    print( f"Simulated reads from B {b_loads}")
    print( f"Simualted stores to C {c_stores}")

if __name__ == "__main__":
    main()
