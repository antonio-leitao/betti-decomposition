FC = gfortran
OPTLEVEL ?= -O3
FCFLAGS = -Wall -Wextra -pedantic -cpp $(OPTLEVEL) #cpp for the preprocessing steps (OpenMP directives)
BUILDDIR = build
TARGET = betti
ifdef OPENMP
    FCFLAGS += -fopenmp
endif
#tell fortran where to search for mod files
FCFLAGS += -J$(BUILDDIR) 

# Default target
all: $(TARGET)

# Ensure the build directory exists
$(BUILDDIR):
	mkdir -p $@

$(TARGET): $(BUILDDIR)/utils_mod.o $(BUILDDIR)/sorted_list_mod.o $(BUILDDIR)/sparse_matrix_mod.o $(BUILDDIR)/simplicial_complex_mod.o $(BUILDDIR)/main.o | $(BUILDDIR)
	$(FC) $(FCFLAGS) -o $@ $^

$(BUILDDIR)/utils_mod.o: src/utils_mod.f90 | $(BUILDDIR)
	$(FC) $(FCFLAGS) -c $< -o $@

$(BUILDDIR)/sparse_matrix_mod.o: src/sparse_matrix_mod.f90 | $(BUILDDIR)
	$(FC) $(FCFLAGS) -c $< -o $@

$(BUILDDIR)/sorted_list_mod.o: src/sorted_list_mod.f90 | $(BUILDDIR)
	$(FC) $(FCFLAGS) -c $< -o $@

$(BUILDDIR)/simplicial_complex_mod.o: src/simplicial_complex_mod.f90 $(BUILDDIR)/sorted_list_mod.o $(BUILDDIR)/sparse_matrix_mod.o $(BUILDDIR)/utils_mod.o | $(BUILDDIR)
	$(FC) $(FCFLAGS) -c $< -o $@

$(BUILDDIR)/main.o: src/main.f90 $(BUILDDIR)/simplicial_complex_mod.o | $(BUILDDIR)
	$(FC) $(FCFLAGS) -c $< -o $@

# New target to run the program
run: $(TARGET)
	./$(TARGET)

clean:
	rm -rf $(BUILDDIR) $(TARGET)


bench:
	@chmod +x tests/benchmark_script.sh
	@$(MAKE) clean
	@$(MAKE) "OPTLEVEL=-O1"
	@./tests/benchmark_script.sh data/sequential_opt1.csv
	@$(MAKE) clean
	@$(MAKE) "OPTLEVEL=-O1"
	@./tests/benchmark_script.sh data/sequential_opt2.csv
	@$(MAKE) clean
	@$(MAKE) "OPTLEVEL=-O3"
	@./tests/benchmark_script.sh data/sequential_opt3.csv


test: $(TARGET)
	./$(TARGET) tests/1_sphere.txt
	./$(TARGET) tests/5_sphere.txt
	./$(TARGET) tests/9_sphere.txt

.PHONY: all clean run bench
