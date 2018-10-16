!************************************
! 3 EN RAYA
!
! 
! Fernando Paniagua. Nov-2016.
! www.fernandopaniagua.com
!************************************
subroutine portada()
	!TEXTO ASCII GENERADO CON http://patorjk.com/software/taag/ 
	print *,"==============================================================================="	
	print *," ______  ____     ___   _____       ___  ____       ____    ____  __ __   ____ "
	print *,"|      ||    \   /  _] / ___/      /  _]|    \     |    \  /    ||  |  | /    |"
	print *,"|      ||  D  ) /  [_ (   \_      /  [_ |  _  |    |  D  )|  o  ||  |  ||  o  |"
	print *,"|_|  |_||    / |    _] \__  |    |    _]|  |  |    |    / |     ||  ~  ||     |"
	print *,"  |  |  |    \ |   [_  /  \ |    |   [_ |  |  |    |    \ |  _  ||___, ||  _  |"
	print *,"  |  |  |  .  \|     | \    |    |     ||  |  |    |  .  \|  |  ||     ||  |  |"
	print *,"  |__|  |__|\_||_____|  \___|    |_____||__|__|    |__|\_||__|__||____/ |__|__|"
	print *,""
	print *,"==============================================================================="
end subroutine portada

subroutine muestratablero(tablero)
	implicit none
	integer tablero(3,3)
	integer i,j
	do i=1,3,1
		print *, tablero(i,1),tablero(i,2),tablero(i,3)
	end do
end subroutine muestratablero

function haytresenraya(tablero)
	implicit none
	logical haytresenraya
	integer tablero(3,3)
	haytresenraya = .FALSE.
	if ((tablero(1,1)>0) .AND. (tablero(1,1)==tablero(2,2)) .AND. (tablero(2,2)==tablero(3,3))) then
		haytresenraya = .TRUE.
	else if ( (tablero(3,1)>0) .AND. (tablero(3,1)==tablero(2,2)) .AND. (tablero(2,2)==tablero(1,3))) then
		haytresenraya = .TRUE.
	else if ((tablero(1,1)>0) .AND. (tablero(1,1)==tablero(1,2)) .AND. (tablero(1,2)==tablero(1,3))) then
		haytresenraya = .TRUE.
	else if ((tablero(2,1)>0) .AND. (tablero(2,1)==tablero(2,2)) .AND. (tablero(2,2)==tablero(2,3))) then
		haytresenraya = .TRUE.
	else if ((tablero(3,1)>0) .AND. (tablero(3,1)==tablero(3,2)) .AND. (tablero(3,2)==tablero(3,3))) then
		haytresenraya = .TRUE.
	else if ((tablero(1,1)>0) .AND. (tablero(1,1)==tablero(2,1)) .AND. (tablero(2,1)==tablero(3,1))) then
		haytresenraya = .TRUE.
	else if ((tablero(1,2)>0) .AND. (tablero(1,2)==tablero(2,2)) .AND. (tablero(2,2)==tablero(3,2))) then
		haytresenraya = .TRUE.
	else if ((tablero(1,3)>0) .AND. (tablero(1,3)==tablero(2,3)) .AND. (tablero(2,3)==tablero(3,3))) then
		haytresenraya = .TRUE.
	end if
end function haytresenraya

program tresenraya
	implicit none
	integer tablero(3,3)!Tablero de juego
	integer i,j !Indices
	integer fila, columna !Posicion de la jugada
	logical jugada_correcta !Indicador de si la jugada en curso es ok
	logical haytresenraya !funcion evalua si hay tres en raya
	real, parameter :: JUGADOR_1 = 1
	real, parameter :: JUGADOR_2 = 2
	integer turno !Indica el turno
	integer jugadas !Numero de jugadas realizadas (max 9)
	turno = JUGADOR_1 !Iniciliza el turno
	jugada_correcta = .FALSE.
	jugadas = 0 !Inicializa las jugadas
	do i=1,3,1 !Inicializa el tablero a 0
		do j=1,3,1
			tablero(i,j)=0
		end do
	end do

	call portada()
	do while((jugadas<9) .AND. (haytresenraya(tablero) .EQV. .FALSE.))
		call muestratablero(tablero)
		jugada_correcta = .FALSE. !Inicializamos para que entre en el bucle do-while
		fila=0 !Inicializamos para que entre en el bucle do-while
		columna=0 !Inicializamos para que entre en el bucle do-while
		do while(jugada_correcta .EQV. .FALSE.) 
			if (turno==JUGADOR_1) then
				print *, "***************"
				print *, "Juega JUGADOR 1"
				print *, "***************"
				print *, "Introduce fila:"
				read *, fila
				print *, "Introduce columna:"
				read *, columna
			else
				print *, "***************" 
				print *, "Juega JUGADOR 2"
				print *, "***************"
				print *, "Introduce fila:"
				read *, fila
				print *, "Introduce columna:"
				read *, columna
			end if
			if ((tablero(fila,columna)==0) .AND. (fila>0) .AND. (fila<4) .AND. (columna>0) .AND. (columna<4)) then
					tablero(fila,columna)=turno
					if (haytresenraya(tablero) .EQV. .TRUE.) then
						print *, "*************************************"
						print *, "*************************************"
						write (*,*) "3 EN RAYA - GANA JUGADOR ", turno
						print *, "*************************************"
						print *, "*************************************"
						call muestratablero(tablero)
						print *, "*************************************"
						print *, "*************************************"
						jugada_correcta = .TRUE.
					else
						if (turno==JUGADOR_1) then 
							turno = JUGADOR_2
						else
							turno = JUGADOR_1
						end if
						jugada_correcta = .TRUE.
						jugadas = jugadas + 1
					end if
				else 
					print *, "ERROR:Celda ocupada o incorrecta. Introduce otra celda"
				end if
		end do
	end do
	if (haytresenraya(tablero) .EQV. .FALSE.) then
		print *, "************************************"
		print *, "************************************"
		print *, "   TABLAS - FINAL DE LA PARTIDA"
		print *, "************************************"
		print *, "************************************"
		call muestratablero(tablero)
		print *, "*************************************"
		print *, "*************************************"
	end if
end program tresenraya