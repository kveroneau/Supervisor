## A tool to monitor and/or control processes

Originally was an ObjectPascal rewrite of the nice http://supervisord.org/, while it
itself has it's strengths, I was looking more for something with minimal dependencies,
low resource use, and programmable.

Welcome to my version of **Supervisord**, written in ObjectPascal and compiles to an ELF binary
just under a megabyte currently.  It only takes about 2MBs of RAM when run, which makes it
well suited for multiple scenarios, such as a simple process control for a Docker container,
a process control program for low spec embedded platforms that support ObjectPascal, or a low
resource process control program for general use.  I made it fairly programmable as well,
and although I wouldn't say it's "production worthy" just yet, it is fairly featureful.

### Modes of operations:

Unlike the project this one was inspired by, this program can also act as a shell program of
sorts.  While not as capable as say bash, it could be used as a simple drop-in when a full
blown shell just isn't needed, such as inside most containers.  The shell component supports
a custom profile file which is used when running commands from the shell.  You can see an
example of what a profile.txt can look like.  This can be used in a Docker container for
example to have a bunch of pre-existing macros which run ObjectPascal routines or other
commands inside the container.

Running the program with no given configuration will drop the user into an ObjectPascal shell.
This shell is very simple in implementation currently, upon every line taken from the user's
preferred input device, the program shall prepend the user's `profile.txt` file, which shall
reside in user's current working directory.  The location of this profile may change in future
versions, be wry of that.

Here is the current list of ObjectPascal procedures which can be called in the shell:

  - procedure WriteLn(const s: string)
  - procedure Halt
  - procedure SetPrompt(const s: string)
  - procedure RunSource(const AFileName: string)
  - procedure Compile(const AFileName: string; const AOutFile: string)
  - procedure Run(const AFileName: string)
  - procedure Exec(const cmd, params: string)
  - procedure RunService(const SName, cmd, params: string)
  - procedure StopService(const SName: string)
  - procedure ListServices
  - procedure Logs(const SName: string)
  - procedure StartHTTPServer(const APort: word)
  - procedure StopHTTPServer
  - procedure StartShell
  - procedure SetPath(const s: string)
  - function GetPath: string
  - procedure SetUser(const uid, gid: integer)
  - procedure SavePID(const fname: string)
  - procedure AlwaysRestart(const SName: string; value: boolean)
  - procedure MonitorServices
  - function GetParams: string
  - procedure EnterDir
  - procedure Help

This procedure list is not finalized, however, do expect API stability through versions.

Most of these procedures and functions should explain themselves.  One which may stick out to
you is `MonitorServices`.  This should be called at the end of setting everything up in code
prior, as this is a blocking call and will never return until there are no more services to
monitor and restart when stopped.  Here's a simple service example:

```
begin
  SavePID('/var/run/Supervisor.pid');
  SetPath('/var/www');
  SetUser(80,80); { Switch for the UID/GID on your system. }
  RunService('HTTPd', '/usr/bin/httpd', ''); { Don't daemonize/fork }
  AlwaysRestart('HTTPd', True);
  MonitorServices;
end.
```

Of course setting UID and GID can only be done if the program is run as root, but Supervisor
can run as either root or a regular user.

The really cool part about this program, is if you do choose to run it inside a docker
container as a process control system, if you run the container as `docker run -it supervisor`
you can interactively start services, monitor them, and stop them by reattaching to the
container.  Just as with the Python-based supervisor, this one also has an integrared web
server, and there is a procedure you can use to both start it and stop it.  So, there are
multiple ways to manage your processes.

### Usage example of PascalShell:

Here is an example `profile.txt` you can check out, this is the exact file I've been using
throughout development as I was testing it as a shell program, and the shell combined with
a proper list of procedures and functions in a profile can really make for a powerful shell
program.

```
procedure edit(const fname: string);
begin
  Exec('/usr/bin/nano', fname);
end;

procedure editProfile;
begin
  edit('profile.txt');
end;

procedure hello(const name: string);
begin
  WriteLn('Hello, '+name+'!');
end;

procedure bash(const params: string);
begin
  Exec('/bin/bash', params);
end;

procedure xterm;
begin
  RunService('XTerm', '/usr/bin/xterm', '');
end;

procedure tail;
begin
  WriteLn('Tailing Supervisor log...');
  Exec('/usr/bin/tail', '-n 10 Supervisor.log');
end;

procedure ls;
begin
  Exec('/bin/ls', '-lh '+GetPath);
end;

procedure cat(const fname: string);
begin
  Exec('/bin/cat', GetPath+'/'+fname);
end;

procedure build;
begin
  WriteLn('Building all binaries...');
  Run('compile.psx');
end;

procedure mem;
begin
  SavePID('/tmp/Supervisor.pid');
  Exec('/bin/bash', '-c "cat /proc/`cat /tmp/Supervisor.pid`/status|grep VmRSS"');
end;

procedure ps;
begin
  ListServices;
  mem;
end;

procedure httpd;
begin
  StartHTTPServer(9001);
end;
```

### Building the project

To build a binary and use this program is pretty trivial.  Before making this repo, I
checked a few popular Linux distributions, and installing the Lazarus ObjectPascal IDE
on each platform is as easy as a few clicks or a package management command away.

Since this program has no external deps, and everything it uses is entirely from the
standard FreePascal and Lazarus component libraries, all you need to really install is
Lazarus for your specific platform, then use the Make program, or run the FPC command
directly to build your binary.  Although, I'd recommend running
`lazbuild --bm=Release Supervisor.lpr` over using the `fpc` command-line, as it greatly
simplifies the build process.  You can also open the `Supervisor.lpi` file in Lazarus
itself and build from there.

I also provided a Makefile which should assist anyone trying to build it with just the
FreePascal compiler and not through Lazarus.  You will just need to replace the location
of the PascalScript component, which is part of the Lazarus source code, in the components
directory.  I'd say using Lazarus is going to be the preferred method of building this.

Finally, you can build it entirely using Docker, on any platform that supports Linux docker
containers.  The provided `Dockerfile` will download a builder container from Docker Hub to
perform the actual build process, then after move the built binary over into a new empty
Debian Buster container, it should also run on most other distros, feel free to experiment,
Debian is not a requirement, it's just my distro of choice.

Since FreePascal and Lazarus are both available for the ARM platform, Supervisor can also
be compiled there as well.  Either using a slightly modified Dockerfile, or by installing
the SDK directly on an ARM device and building.
