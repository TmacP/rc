export PS1='ὁ δὲ ἀνεξέταστος βίος οὐ βιωτὸς ἀνθρώπῳ > '
# Aliases
alias f='find -name "*.ts" | xargs grep -i'
alias ed="rlwrap -c -D 2 -r ed -E -p '> '"
alias class='cd ~/Documents/classes_winter2025'
alias 370='cd /student/trm003/Documents/classes_winter2025/cmpt370'
alias 340='cd /student/trm003/Documents/classes_winter2025/cmpt340'
alias 317='cd /student/trm003/Documents/classes_winter2025/cmpt317'
alias 280=' cd /student/trm003/Documents/classes_summer2025/cmpt280Summer2025/'
alias dt='dtach -A /student/trm003/cmpt -z bash'
alias at='dtach -a /student/trm003/cmpt -z '
alias yes=''
alias dt332='dtach -A /student/trm003/cmpt332 -z bash'
alias at332='dtach -a /student/trm003/cmpt332 -z '
alias dt280='dtach -A /student/trm003/cmpt280 -z bash'
alias at280='dtach -a /student/trm003/cmpt280 -z '
alias dtds='dtach -A /student/trm003/cmptds -z bash'
alias atds='dtach -a /student/trm003/cmptds -z '
alias tux='ssh trm003@tux6'
alias j='fgmenu'
alias cl='clear;ls'
alias gdc='cd /student/trm003/Documents/UGDC'
alias 332='cd /student/trm003/Documents/classes_fall2025/cmpt332/xv6-riscv/'
alias pd='pushd'
alias tr='tput rc;tput cuu 1; tput ed'
alias tu='tput cuu 2'
alias ts='tput sc'
alias gs='git status -sb'
alias gd='git diff'
alias gc='git commit -p'
alias gp='git pull && git push'

export PATH="/usr/local/bin:$HOME/.local/bin:$HOME/.cargo/bin:$PATH"
 

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

function d() {
  # Get directory list and count before printing
  local dirlist
  dirlist=$(dirs -v)
  local dircount=$(echo "$dirlist" | wc -l)

  # Print the list and prompt
  echo "$dirlist"
  printf "Enter directory number: "

  # Save cursor position *after* list and prompt
  tput sc
  read -r choice

  # Restore position, move the cursor to the left, clear the current line and dir list
  tput rc
  tput cuu $((dircount + 1))  # Move up by the number of lines in the directory list + 1 for the prompt
  tput cr  # Move the cursor to the beginning of the line
  tput el  # Clear the current line (the one with the prompt)
  tput ed  # Clear everything below the cursor position

  # Do the actual pushd
  if [[ "$choice" =~ ^[0-9]+$ ]]; then
    pushd +"$choice" > /dev/null || echo "Failed to rotate directory stack."
  else
    echo "Invalid choice: $choice"
  fi
}

function fgmenu() {
  # Save current cursor position
  tput sc

  # Get and clean job list
  local joblist
  joblist=$(jobs -s | \
    sed -E "s/Stopped *//; 
s|rlwrap -c -D 2 -r ed -E -p '> ' ||
            s| *\(wd:.*\)||")

  local jobcount=$(echo "$joblist" | wc -l)

  # Print cleaned job list and get input
  echo "$joblist"
  read -p "fg %: " jobnum

  # Move up job list + 1 for the prompt
  tput cuu $((jobcount + 2))
  tput cr
  tput el

  # Clear below and restore
  tput ed

  # Foreground the selected job
  fg %"$jobnum"
}


function t() {
    tput cup "$1"
    tput ed
}
# pretty‐print a source file in less with pygmentize + line wrapping
function pp() {
  local file=$1
  local ext=${file##*.}
  local lang

  case "$ext" in
    c)        lang=C ;;
    h)        lang=C ;;
    py)       lang=python ;;
    js)       lang=javascript ;;
    ts)       lang=typescript ;;
    java)     lang=java ;;
    sh)       lang=bash ;;
  esac

  pygmentize -l "$lang" -O style=zenburn "$file" | less -R
}

export PATH="$HOME/.local/bin:$PATH"

export PATH=~/local/llvm/bin:$PATH
