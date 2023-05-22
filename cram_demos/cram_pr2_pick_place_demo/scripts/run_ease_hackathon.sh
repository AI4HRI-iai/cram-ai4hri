cram_entrypoint_file=$(echo "$(rospack find cram_pr2_pick_place_demo)/scripts/ease-hackathon-entrypoint.lisp")

/usr/bin/sbcl --dynamic-space-size 8192 --noinform --disable-debugger --load $cram_entrypoint_file --quit
