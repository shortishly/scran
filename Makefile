#-*- mode: makefile-gmake -*-
# Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

PROJECT = scran
PROJECT_DESCRIPTION = Parser Combinators
PROJECT_VERSION = ${shell git describe --tags}

COVER = 1
COVER_REPORT_DIR = _site/cover

SHELL_DEPS = \
	sync

SHELL_OPTS = \
	+pc unicode \
	-config dev.config \
	-enable-feature maybe_expr \
	-s sync

EUNIT_ERL_OPTS = \
	-enable-feature maybe_expr

PLT_APPS = \
	compiler \
	crypto \
	stdlib

EDOC_OPTS = {preprocess, true}, {dir, "_site/edoc"}

define HEX_TARBALL_EXTRA_METADATA
#{
	licenses => [<<"Apache-2">>],
	links => #{
		<<"Function reference">> => <<"https://shortishly.github.io/scran/edoc/">>,
		<<"Test Coverage">> => <<"https://shortishly.github.io/scran/cover/">>,
		<<"GitHub">> => <<"https://github.com/shortishly/scran">>
	}
}
endef

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

app:: rebar.config
