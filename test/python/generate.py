from neo4j._codec.packstream.v1 import Packer

packable_buffer = Packer.new_packable_buffer()
packer = Packer(packable_buffer)

def generate(name : str, v):
  packable_buffer.clear()
  packer.pack(v)
  with open(f"{name}.bin", "wb") as f:
    f.write(packable_buffer.data)

def truth():
  generate("truth", True)

def untruth():
  generate("untruth", False)

def serially_increasing_list():
  l = [i for i in range(100_000)]
  generate("serially_increasing_list", l)

def small_dictionary():
  d = { "pack": "stream", "12": 34, "true": False }
  generate("small_dictionary", d)

def string():
  generate("string", "Hello world")

def float_():
  generate("float", 1.234)

def nan():
  generate("nan", float("NaN"))

def inf():
  generate("inf", float("Infinity"))

def neg_inf():
  generate("neg_inf", float("-Infinity"))

all_tests = [
  truth,
  untruth,
  serially_increasing_list,
  small_dictionary,
  string,
  float_,
  nan,
  inf,
  neg_inf
]

if __name__ == "__main__":
  for f in all_tests:
    f()
