#!/bin/zsh

# specify the directory to search (default to current directory)
search_dir="${1:-.}"

# find files (excluding binary files) and process them
fd --type f --search-path "$search_dir" --exec zsh -c '
  file="$1"
  total_lines=$(wc -l < "$file")
  if [[ $total_lines -eq 0 ]]; then
    return # skip empty files
  fi

  comment_lines=$(gawk "/^[[:space:]]*\\/\\// {count++} END {print count + 0}" "$file")

  # calculate percentage of comment lines
  percent=$((100 * comment_lines / total_lines))

  # print file if more than 30% of lines are comments
  if [[ $percent -gt 30 ]]; then
    echo "$file ($percent% comments)"
  fi
' sh {} \;
