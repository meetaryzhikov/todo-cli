# Todo CLI

A simple command-line TODO application written in Common Lisp.

## Installation

1. Ensure you have a Common Lisp implementation (e.g., SBCL) and ASDF installed.
2. Clone the repository:
   ```bash
   git clone https://github.com/meetaryzhikov/todo-cli.git
   cd todo-cli
   ```
3. Install dependencies:
   ```bash
   # Using Quicklisp (recommended)
   (ql:quickload :alexandria)
   (ql:quickload :cl-fad)
   (ql:quickload :split-sequence)
   (ql:quickload :local-time)
   ```
4. Build the application:
   ```bash
   sbcl --load todo-cli.asd --eval "(asdf:make :todo-cli)" --quit
   ```
5. Add the `bin/` directory to your PATH or copy `bin/todo` to a directory in your PATH.

## Usage

```bash
todo <command> [arguments]
```

## Commands

- `add <title> [description]` - Add a new task
- `list [--all]` - Show task list (use --all to include completed tasks)
- `complete <id>` - Mark task as completed
- `remove <id>` - Delete a task
- `help` - Show help information

Tasks are stored in `~/.todos.lisp`.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
