FC = gfortran
FCFLAGS = -Wall -Wextra -pedantic
BUILDDIR = build
TARGET = betti
#tell fortran where to search for mod files
FCFLAGS += -J$(BUILDDIR) 

# Default target
all: $(BUILDDIR)/$(TARGET)

# Ensure the build and module directories exist
$(BUILDDIR):
	mkdir -p $@

$(BUILDDIR)/$(TARGET): $(BUILDDIR)/utils_mod.o $(BUILDDIR)/sorted_list_mod.o $(BUILDDIR)/sparse_matrix_mod.o $(BUILDDIR)/simplicial_complex_mod.o $(BUILDDIR)/main.o | $(BUILDDIR)
	$(FC) $(FCFLAGS) -o $@ $^

$(BUILDDIR)/utils_mod.o: src/utils_mod.f90 | $(BUILDDIR)
	$(FC) $(FCFLAGS) -c $< -o $@

$(BUILDDIR)/sparse_matrix_mod.o: src/sparse_matrix_mod.f90 | $(BUILDDIR)
	$(FC) $(FCFLAGS) -c $< -o $@

$(BUILDDIR)/sorted_list_mod.o: src/sorted_list_mod.f90 | $(BUILDDIR)
	$(FC) $(FCFLAGS) -c $< -o $@

$(BUILDDIR)/simplicial_complex_mod.o: src/simplicial_complex_mod.f90 $(BUILDDIR)/sorted_list_mod.o $(BUILDDIR)/sparse_matrix_mod.o $(BUILDDIR)/utils_mod.o| $(BUILDDIR)
	$(FC) $(FCFLAGS) -c $< -o $@

$(BUILDDIR)/main.o: src/main.f90 $(BUILDDIR)/simplicial_complex_mod.o | $(BUILDDIR)
	$(FC) $(FCFLAGS) -c $< -o $@

# New target to run the program
run:
	./$(BUILDDIR)/$(TARGET)

clean:
	rm -rf $(BUILDDIR)

bench:
	./$(BUILDDIR)/$(TARGET) benchmarks/barabasi.txt

.PHONY: all clean run
