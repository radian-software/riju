# Fast Sandbox

This project is a work in progress and does not contain any serious
documentation.

## API

    POST /api/v1/ws?lang=python

The API is based on message passing.

### Server messages

Received output from process.

    {
      "event": "terminalOutput",
      "output": ">>> "
    }

Package name completions.

    {
      "event": "packageNameCompletions",
      "packageNameCompletions": ["Flask", "Flask-Talisman"],
      "messageSerial": 42
    }

### Client messages

Received input from user.

    {
      "event": "terminalInput",
      "input": "print('Hello, world!')\n"
    }

User wants to run code.

    {
      "event": "runCode",
      "code": "import this"
    }

User wants to install a package.

    {
      "event": "installPackage",
      "packageName": "Flask"
    }

Complete package names.

    {
      "event": "completePackageName",
      "partialPackageName": "fla",
      "messageSerial": 42
    }
