# We need


- To be able to apply N qubit gates, where N is less than the number of total qubits
- A way to apply function to some given number of qubits
- If G is a 1-qubit gate and, then get(0, (G \ox I))(a, M) = G(a)

# An idea

Represent linear functions as matrices. Implement an operation that takes the tensor product of two linear functions.
Then, in order to get the "types" to line up, tensor with the identity operation on the left and right as needed.
This can then be translated into the appropriate SMT operations.

# Related

Look at Conal's work on automatic differentiation and convolution: there might be some useful things about tensoring linear functions there.
