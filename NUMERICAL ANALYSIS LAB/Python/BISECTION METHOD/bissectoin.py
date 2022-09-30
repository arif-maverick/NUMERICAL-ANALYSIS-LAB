import math
def f(x):
    return x*math.exp(x)-1

def bisection(a,b,e):
    step = 1
    print('\n\n---Bisection Method---')
    condition = True
    while condition:
        p = (a + b)/2
        print('Iteration-%d, p = %0.6f and f(p) = %0.6f' % (step, p, f(p)))

        if f(a) * f(p) < 0:
            b = p
        else:
            a = p
        
        step = step + 1
        condition = abs(f(p)) > e

    print('\nRequired Root is : %0.8f' % p)



a = input('First Guess: ')
b = input('Second Guess: ')
e = input('Tolerable Error: ')


a = float(a)
b = float(b)
e = float(e)


if f(a) * f(b) > 0.0:
    print('Given guess values do not bracket the root.')
    print('Try Again with different guess values.')
else:
    bisection(a,b,e)