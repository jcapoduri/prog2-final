unit lib.images;

interface

uses
  sysutils, md5, fileutil;

const
  BASEPATH = 'data/images';

type
  tImageContainer = record
                      path: string;
                    end;

  procedure setupContainer (var this : tImageContainer; path : string);
  procedure storeImage     (var this : tImageContainer; pathToImage : string);
  function  retrieveImage  (var this : tImageContainer; uniqId : string) : string;

implementation
  function _uniqId (image : string) : string;
  begin
    _uniqId := MD5Print(MD5File(image));
  end;

  procedure setupContainer (var this : tImageContainer; path : string);
  begin
    this.path := path;
  end;

  procedure storeImage     (var this : tImageContainer; pathToImage : string);
  var
    filename : string;
  begin
    filename := _uniqId(pathToImage);
    copyfile(pathToImage, retrieveImage(this, filename));
  end;
  
  function  retrieveImage  (var this : tImageContainer; uniqId : string) : string;
  begin
    retrieveImage := this.path + '/' + uniqId;
  end;
  

end.
