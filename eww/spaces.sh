spaces_query=$(yabai -m query --spaces --display)
output="<box class='aaaa'>"
for space in $(echo $spaces_query | jq -c '.[]')
do
	if [ $(echo $space | jq .visible) = 1 ]
	then
		output+="
		<button class='visible-space'>
			$(echo $space | jq .index)
		</button>
		"
	else
		output+="
		<button class='space' onclick='yabai -m space --focus $(echo $space | jq .index)'>
			$(echo $space | jq .index)
		</button>
		"
	fi
done
output+="</box>"
echo $output
