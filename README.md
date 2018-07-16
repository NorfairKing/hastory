# Hastory

Hastory keeps track of your terminal history and uses it to optimize the way you use your terminal.
For example, it allows you to quickly jump to one of the directories you use the most.

## Installing the `hastory` binary from source

First get the hastory sources:

``` shell
git clone https://github.com/NorfairKing/hastory
```

You can install hastory with [`stack`](https://haskellstack.org/)

``` shell
cd hastory
stack install
```

## Installing the Hastory harness

To start recording your history with `hastory`, we need to hook into your shell's functionality to record every command you execute.

With `hastory generate-gather-wrapper-script`, we can generate this script automatically.
This means that the only thing you need to do is to add a one-liner to the script that is loaded on your shell's startup script.
For bash, it suffices to add the following to .bashrc.

``` shell
PROMPT_COMMAND="hastory_gather_"
```

On the other hand, hastory works for zsh if you add this line to .zshrc.

``` shell
precmd() { hastory_gather_ }
```

When you restart your shell (for example by restarting your terminal), you should see history accumulating in `~/.hastory/command-history/$(date +%F).log`.

Note: Feel free to make sure that `hastory generate-gather-wrapper-script` is code that you actually want to run.

## Frequency-based directory changes

> The ease of performing a task should be proportional to its frequency.

Using your command history, `hastory` tries to predict the N directories that you are most likely to want to jump to.

To use this feature, you need to hook into your shell again.
With `hastory generate-change-directory-wrapper-script`, we can generate this script automatically.

To source this script every time, put the following in your shell's startup script. (`.bashrc` for bash, `.zshrc` for zsh, for example.)

``` shell
source <(hastory generate-change-directory-wrapper-script)
```

Now you can run `hastory_change_directory_` to list the directories that you might want to jump to.
The output should look something like this:

``` shell
/home/user $ hastory_change_directory_
0  /home/user/work/
1  /home/user/a/very/deep/directory/tree/that/i/use/often
2  /home/user/archive
3  /home/user/backup
```

If the directory that you want to jump to is in the list with index `i`, you can jump to it with `hastory_change_directory_ i`.

``` shell
/home/user $ `hastory_change_directory_ 1
/home/user/a/very/deep/directory/tree/that/i/use/often $ 
```

Now you probably want to alias that long command `hastory_change_directory_` to something shorter like `f`, as follows:

``` shell
alias f=hastory_change_directory_
```

Note: Feel free to make sure that `hastory generate-change-directory-wrapper-script` is code that you actually want to run.

## What hastory stores

In order to perform the above described features, hastory must keep a log of some things.
The logs are all stored in ``~/.hastory``.
The raw logs are stored in ``~/.hastory/command-history/$(date +%F).log``.
These files contain records of the commands you executed, when and in which directory they were executed as well as some extra information.
The most list of most useful directories is cached in ``~/.hastory/recent-dirs-cache.json``.
