c Noel Kennebeck
c Assignment 1
c This program simulates a game of mastermind
      
			PROGRAM mastermind
			IMPLICIT none

c Define all variables
			integer colors, pegs, rplace, wplace, numgoes, seed, randint
			integer k, i, t, j, p
			integer values (8)
			real rand1
			character temp
			character (3) playagain
			character (6) guess, solution, tempsol
			character (26) upper, lower

			upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
			lower = 'abcdefghijklmnopqrstuvwxyz'

			call date_and_time (values = values)
			seed = values(8)
			call srand(seed)

c Begin game play
			write(*,2)
		  read(*,*)
990		write(*,3)
			numgoes = 0
			read(*,*) colors, pegs
			
			IF(colors.EQ.1) THEN
			 write(*,*) 'Color is (R)ed'
			ELSE IF(colors.EQ.2) THEN
			 write(*,*) 'Colors are (R)ed and (G)reen'
			ELSE IF(colors.EQ.3) THEN
			 write(*,*) 'Colors are (R)ed, (G)reen, and (B)lue'
			ELSE IF(colors.EQ.4) THEN
			 write(*,*) 'Colors are (R)ed, (G)reen, (B)lue, and (Y)ellow'
			ELSE IF(colors.EQ.5) THEN
			 write(*,4)
			 write(*,*) 'and (O)range'
			ELSE IF(colors.EQ.6) THEN
			 write(*,4)
			 write(*,*) '(O)range, and (P)urple'
			ELSE
			 write(*,50)
			 GO TO 990
			END IF
		
			IF((pegs.lt.1).or.(pegs.gt.6)) THEN
			 write(*,50)
			 GO TO 990
			END IF
			
c	Generate random solution
      DO k = 1, pegs
			 rand1 = rand(seed)
			 seed = rand1
			 randint = MOD((rand1 * 10), (colors*1.0)) + 1.0
			 IF (randint.eq.1) THEN
			  solution(k:k) = 'r'
			 ELSE IF (randint.eq.2) THEN
				solution(k:k) = 'g'
			 ELSE IF (randint.eq.3) THEN
				solution(k:k) = 'b'
			 ELSE IF (randint.eq.4) THEN
				solution(k:k) = 'y'
			 ELSE IF (randint.eq.5) THEN
				solution(k:k) = 'o'
			 ELSE IF (randint.eq.6) THEN
				solution(k:k) = 'p'
			 END IF
			END DO

c	User makes a guess
991		write(*,5)
			read(*,14) guess
			numgoes = numgoes + 1
			rplace = 0
			wplace = 0
			tempsol = solution

c Make lower case
			DO i = 1, pegs
			 t = index(upper, guess(i:i))
			 IF(.not.(t.eq.0)) THEN
				guess(i:i) = lower(t:t)
			 END IF
			END DO

c Check right place, right color
			DO i = 1, pegs
			 temp = guess(i:i)
			 t = index(tempsol, temp)
			 IF(t.eq.i) THEN
			  rplace = rplace + 1
				tempsol(t:t) = 'x'
				guess(t:t) = 'z'
			 END IF
			END DO

c Check wrong place, right color
			DO i = 1, pegs
			 temp = guess(i:i)
			 t = index(tempsol, temp)
			 IF(t.ne.0) THEN
				wplace = wplace + 1
				tempsol(t:t) = 'x'
				guess(i:i) = 'z'
			 END IF
			END DO

c Check if solved
			IF(rplace.eq.pegs) THEN
			 GO TO 992
			END IF

c Report back to user number of right place and right color, wrong place
			write(*,6)
			IF(rplace.eq.1) THEN
			 write(*,7)
			ELSE
			 write(*,15,advance='no') rplace
			 write(*,8)
			END IF
			IF(wplace.eq.1) THEN
			 write(*,9)
			ELSE
  		 write(*,15,advance='no') wplace
       write(*,10)
			END IF

c	Prompt for next guess
			GO TO 991

c User has solved the puzzle!
992		write(*,11)
			write(*,15,advance='no') numgoes
			IF(numgoes.eq.1) THEN
			 write(*,*) 'go.'
			ELSE
			 write(*,*) 'goes.'
			END IF
		  write(*,12)
993		read(*,14) playagain

c Make response lowercase and check
			DO j = 1, 3
			 p = index(upper, playagain(j:j))
			 IF(p.ne.0) THEN
			  playagain(j:j) = lower(p:p)
			 END IF
			END DO
			IF(playagain.eq.'no') THEN
			 GO TO 999
			ELSE IF(playagain.eq.'yes') THEN
			 GO TO 990
			ELSE
			 write(*,13)
			 GO TO 993
			END IF

c	FORMAT STATEMENTS
2     FORMAT ('Welcome to Mastermind. (Press enter to continue): ', $)
3			FORMAT ('How many colors and pegs should I use? ', $)
4			FORMAT ('Colors are (R)ed, (G)reen, (B)lue, (Y)ellow,', $)
5			FORMAT ('Enter your guess: ', $)
6			FORMAT ('Your guess contains', $)
7			FORMAT (' 1 piece in the right place and', $)
8			FORMAT (' pieces in the right place and', $)
9			FORMAT (' 1 piece of the correct color in the wrong place')
10		FORMAT (' pieces of the correct color in the wrong place')
11		FORMAT ('You guessed it in', $)
12		FORMAT ('Thank you. Play again? ', $)
13		FORMAT ('Please enter yes or no. Play again? ', $)
14		FORMAT (A)
15		FORMAT (I2)
50		FORMAT ('Please enter a valid input.')

999   END
