PROGRAM bisection
    implicit none
    real a,b,e,p
    write(*,*)"Provide the [a,b] a and b"
    read(*,*)a,b
    write(*,*)"The tolerance value of e"
    read(*,*)e
    call bisect(a,b,p)
    write(*,*)'THE ROOT OF THIS FUNCTION is',p


END PROGRAM bisection

real function f(x)
    implicit none
    real x
    f(x)=x*exp(x)-1
end function

subroutine bisect(a,b,p)
    implicit none
    integer step
    real a,b,e,p,f
    logical condition
    step=1
    write(*,*) 'BISECTION METHOD    IMPLEMENTATION '
    condition=.TRUE.
    do while (condition)
        p=(a+b)/2
        write(*,*)step,b,f(b)

        if (f(a)*f(p)<0)then
            a=p
        else
            b=p
        end if
        step=step+1
        condition = abs((a-b)/p)>e
    end do



end subroutine

