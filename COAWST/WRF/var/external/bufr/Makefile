.SUFFIXES: .c .f .for .o

include make.filelist

AR = ar
ARFLAGS = cr
TARGET = libbufr.a

library:$(OBJ)
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)
	$(RANLIB) $(TARGET)

.c.o:
	$(RM) $@
	$(CC) -o $@ -c $(CFLAGS) -DUNDERSCORE $*.c

.f.o:
	$(FC) -c $(FFLAGS) $< 

.for.o:
	./preproc.sh $*.for
	$(FC) -c $(FFLAGS) $*.f

clean:
<<<<<<< HEAD
	rm -f $(OBJ) $(TARGET) irev.f wrdlen.f
=======
	@rm -f $(OBJ) $(TARGET) irev.f wrdlen.f
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834

superclean: clean
