all:
	@erlc -o ./ ../src/*.erl

clean:
	@rm -rf ./*.beam

