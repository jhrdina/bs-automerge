module Js = AMJsRe;

module type CommonAPI = CommonAPI.CommonAPI;
module UniJs: {module Make: CommonAPI.Maker;} = AMUniJsRe;
module Pure = AMPure;
/* module UniPure: CommonAPI.CommonAPI = AMPure; */