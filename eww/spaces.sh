spaces_query=$(yabai -m query --spaces --display)
windows=true
output="<box class='aaaa'>"
for space in $(echo $spaces_query | jq -c '.[]')
do
	if [ $(echo $space | jq .visible) = 1 ]
	then
		button_properties="class='visible-space'"
	else
		button_properties="class='space' onclick='yabai -m space --focus $(echo $space | jq .index)'"
	fi

	output+="
	<button $button_properties>
		$(echo $space | jq .index)
	</button>
	"
done
output+="</box>"
echo $output
