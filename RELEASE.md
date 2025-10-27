decay is an experimental, self-mutating programming language where code gradually deteriorates over time. This decay challenges developers to write resilient, self-healing programs and think critically about long-term program stability.

## See It In Action

Watch the demo video to see some of the examples work without having to download it for yourself

<p align="center">
  <a href="https://hc-cdn.hel1.your-objectstorage.com/s/v3/70a072e0399f5b4f6392d9e4a9f6e8d911ced1cd_decay-demo.mp4">
    <img src="https://dummyimage.com/800x450/000/fff&text=▶+Watch+Decay+Demo" alt="Watch the Decay Demo Video" width="600"/>
  </a>
</p>

## Getting Started

1. **Download** the decay binary from this releases page if you're on Linux, otherwise build it yourself(Requires sbcl):
```bash
   sbcl --load src/main.lisp --eval "(save-lisp-and-die \"decay\" :toplevel (lambda () (main *posix-argv*)) :executable t)"
```

2. **Run Examples** to see decay in action
