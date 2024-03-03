# Development Plan

1. Utilize the VQO code (https://github.com/inQWIRE/VQO) to make a Haskell interpreter for an effective quantum simulator.
2. Build symbolic interpreter of Qafny on top of VQO.
3. Develop other subset of quantum operators that can be effectively simulatable.

# Example Benchmarks

1. Be able to verify by symbolically testing Shor's algorithm. We can claim that we are able to simulate the whole Hidden-sub-group problems
2. Amplitude amplification and estimation (https://arxiv.org/abs/1904.10246, https://arxiv.org/pdf/quant-ph/0005055.pdf).
3. Quantum signal processing

The last two require additional subset finding. One candidate subset is in the "amplitude estimation without phase estimation" paper. It discusses the use of consecutive Ry gates for constructing the diffusion operators. We can define a type based on the type system in VQO to only allow users to write Ry gates.
