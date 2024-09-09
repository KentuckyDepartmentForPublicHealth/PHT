count=1
for file in *; do
  mv "$file" "$(printf "%02d" $count)_$file"
  count=$((count + 1))
done
