!PER PARALLELIZZARE USA SEMPRE L'ESTENSIONE .F90, NON .f90!
!LINEA DI COMPILAZIONE PER IL CODICE SEQUENZIALE: gfortran parallelPi.F90 -fopenmp -DSEQUENCIAL
!LINEA DI COMPILAZIONE PER IL CODICE PARALLELO: gfortran parallelPi.F90 -fopenmp -DPARALLEL_VERSION
program pigreco
    !----------------------------------------!
    use OMP_LIB
    implicit none
    !----------------------------------------!
    integer :: i
    integer, parameter :: N = 100000
    integer, parameter :: NCPU = 4 !numero di CPU utilizzate
    real*8 :: t0, t1 !tempi da utilizzare, *8 per evitare un warning
    real :: h, totale, x, f
    !----------------------------------------!
    print '(a,2x,i15)', ' Number of intervals: ', N
    totale = 0.0
    h = 1. / N

    call OMP_SET_NUM_THREADS(NCPU)
    write(*, '(a,i10)') 'Numero di processori totali: ', NCPU
    
    t0 = OMP_GET_WTIME()
    !----------------------------------------!
#ifdef SEQUENCIAL
    !
    print '(a)', "Scelta la versione sequenziale."
    !
    do i = 1, N 
        x = (i - 0.5) * h
        f = (4 * h) / (1 + x**2)
        totale = totale + f
    enddo
    !
#elif PARALLEL_VERSION
    !
    print '(a)', "Scelta la versione parallela."
    !
    !----------------------------------------!
    !do private rende le variabili x e f non condivise e quindi impedisce che vengano sovrascritte
    !in assenza del reduction prima di calcolare il totale per fare la somma bisognerebbe utilizzare un $OMP CRITICAL
    !e in seguito $OMP END CRITICAL (v. pigreco.F90) 
    !----------------------------------------!
    !
    !$OMP PARALLEL DO PRIVATE(x, f) REDUCTION(+:totale)
    !
    do i = 1, N 
        x = (i - 0.5) * h
        f = (4 * h) / (1 + x**2)
        totale = totale + f
    enddo
    !$OMP END PARALLEL DO
    t1 = OMP_GET_WTIME()
    !
#endif
    !
    t1 = OMP_GET_WTIME()
    !
    PRINT '(a,2x,f30.25)', ' Computed PI =', totale
    PRINT '(a,2x,f30.25)', ' Total computational time =', t1 - t0
    !
end program pigreco