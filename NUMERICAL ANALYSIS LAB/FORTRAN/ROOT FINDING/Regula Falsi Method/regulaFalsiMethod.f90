PROGRAM bisection
    implicit none
    real::a,b,root
    write(*,*)"Write the value of interval a and b"
    read(*,*)a,b
    call bisect(a,b,root)
    write(*,*)'THE ROOT OF THIS FUNCTION is',root
END PROGRAM

real function f(x)
implicit none
real:: x
 f=4*exp(-(x))*sin(x)-1
end function
subroutine bisect(a,b,root)
    implicit none
    real::a,b,p,tol,error,root,f
    write(*,*)"Enter Tolerance"
    read(*,*)tol
    999  p=(a*f(b)-b*f(a))/(f(b)-f(a))

    if(f(p)*f(b)<0)then
        a=p
    else
        b=p
    end if

    error =abs ((a-b)/p)

    if(error<tol)then
        root =p
       else
            goto 999
    end if
end subroutine


