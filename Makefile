FC = gfortran
FCFLAGS = -Wall -Wextra -pedantic -O3
BUILDDIR = build
TARGET = betti
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

bench: $(TARGET)
	./$(TARGET) benchmarks/barabasi.txt

test: $(TARGET)
	./$(TARGET) tests/1_sphere.txt
	./$(TARGET) tests/5_sphere.txt
	./$(TARGET) tests/9_sphere.txt
	./$(TARGET) tests/example.txt

.PHONY: all clean run bench
