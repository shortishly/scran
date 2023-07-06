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
PROJECT_VERSION = 0.1.0

SHELL_DEPS = \
	sync

SHELL_OPTS = \
	-config dev.config \
	-s sync \
	+pc unicode

PLT_APPS = \
	compiler \
	crypto \
	stdlib

PLT_APPS = \
	asn1 \
	compiler \
	crypto \
	inets \
	mnesia \
	public_key \
	runtime_tools \
	ssl \
	stdlib \
	syntax_tools \
	tools

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

app:: rebar.config
