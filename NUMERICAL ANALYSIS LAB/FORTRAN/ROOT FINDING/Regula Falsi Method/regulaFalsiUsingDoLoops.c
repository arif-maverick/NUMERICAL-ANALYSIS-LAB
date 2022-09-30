PROGRAM bisection
    IMPLICIT NONE
    REAL :: a,b,tol,er,p,f
    INTEGER :: i=0
    WRITE (*,*) 'Enter [a,b] of the given equation'
    READ (*,*) a,b
    WRITE (*,*) 'The tolerance up to'
    READ (*,*) tol

    IF ((f(a)*f(b))== 0) THEN
        IF ((f(a)==0).AND.(f(b)==0)) THEN
            WRITE (*,*) 'The roots are',a,' and ',b
        ELSE IF (f(a)==0) THEN
            WRITE (*,*) 'The root is',a
        ELSE IF (f(b)==0) THEN
            WRITE (*,*) 'The root is',b
        END IF
    ELSE IF ((f(a)*f(b))>0) THEN
        WRITE (*,*) 'The root does not exist in  ',a,' and ',b
    ELSE IF ((f(a)*f(b))<0) THEN
        DO
            p=(a*f(b)-b*f(a))/(f(b)-f(a))
            IF (f(p)==0) EXIT
            er=abs((p-a)/p)
            IF (er<((tol))) EXIT
            IF ((f(p)*f(a))<0) THEN

                b=p
            ELSE IF ((f(p)*f(b))<0) THEN
                a=p

            END IF
            i = i + 1
        END DO
        WRITE (*,*) 'The root is',p
        WRITE (*,*) 'Total Steps =',i
   END IF

END PROGRAM

		REAL FUNCTION f(x)

			REAL::x
			f=x*EXP(x)-1
		END FUNCTION f



