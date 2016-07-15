#ifndef IDENTIFIABILITY_HPP
#define IDENTIFIABILITY_HPP

#include <ginac/symbol.h> 
#include <vector> 
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/multi_array.hpp>
#include "mathtree.hpp"
#include "helper_types.hpp"
#include "modelstructure.hpp"


typedef std::pair<boost::shared_ptr<MathTree::parameter>,GiNaC::ex> parametermap;

class parameterlist : public std::vector<parametermap> {
public: 
  boost::shared_ptr<MathTree::parameter> getParameterForExpression(GiNaC::ex e);
};

void identifiability_analysis(
  // Output: Entries of the matrix after having removed non-identifiable parameters flattened (row by row).
  equation_matrix &output_matrix, 
  // Output: Vector containing all new parameters
  std::vector<GiNaC::ex> &paths,
  std::vector<MathTree::parameter::Ptr> &parameters,
  // Output: Dependency of new parameters on old parameters (and vice versa...)
  double_matrix &parameter_dependency_matrix,
  // Output: Dependency of new parameters on old parameters (not in rref)
  int_matrix &parameter_dependency_matrix_unreduced,

  // Input: Matrix of expression
  const symbolic_matrix &input_matrix, 
  // Input: vector of symbols which are the parameters in the input-matrix
  std::vector<GiNaC::symbol> & vars,
  // Input: the structure of the network, which will be modified to limit the number of -1 exponents
  ModelStructure &structure);

MathTree::math_item::Ptr put_into_mathtree_format (GiNaC::ex e, parameterlist &param, bool reduce_products=true);
template<typename MatrixType> void remove_minus_one(MatrixType &A, ModelStructure& structure, std::vector<GiNaC::symbol> & vars, size_t size);

#endif // IDENTIFIABILITY_HPP
