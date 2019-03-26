module Js = AMJsRe;

module type CommonAPI = CommonAPI.CommonAPI;
module UniJs: {module Make: CommonAPI.Maker;} = AMUniJsRe;
/* module UniPure: CommonAPI.CommonAPI = AMPure; */