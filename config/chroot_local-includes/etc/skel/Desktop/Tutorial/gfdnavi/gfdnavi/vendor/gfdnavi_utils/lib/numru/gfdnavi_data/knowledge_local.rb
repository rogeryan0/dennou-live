# -*- coding: cp932 -*-
require "numru/gfdnavi_data/knowledge"
require "forwardable"

module NumRu::GfdnaviData
  class KnowledgeLocal < NumRu::GfdnaviData::Knowledge
    extend Forwardable

    OBJECT_CLASS = ::Knowledge

    # * �唼�̃��\�b�h�� Knowledge �I�u�W�F�N�g�Ɉڏ�����
    #   (models/knowledge.rb �̓����̃��\�b�h�Ɠ�������������悤�ɂ���)
    #
    #if Node.table_exists? && OBJECT_CLASS.table_exists?
    #meths = Node.column_names.dup + OBJECT_CLASS.column_names.dup
    if OBJECT_CLASS.table_exists?
      meths = OBJECT_CLASS.column_names.dup
      meths.delete("id")
      meths.collect!{|me| /\A(.+)_id\z/ =~ me ? $1 : me} # ...._id �� _id ������
      meths += ["get_contents", "relational_images", "relational_variables", "figures", "insert_figures", "swap_figures", "comments", "make_new_comment", "save", "delete", "delete_figure"]
      # figures= �� Local �̂݁BRemote �� add_figure ���g���B
      meths.each do |me|
        def_delegator :@object, me
        def_delegator :@object, me+"="
      end
    end

    def errors
      @object.errors.full_messages
    end

    def to_knlge
      # type�̃`�F�b�N���K�v
      path = @object.path
      if /\A\/usr(\/.*)\Z/ =~ path
        path = GFDNAVI_USER_PATH + $1
      else
        path = GFDNAVI_DATA_PATH + path
      end
      knowledge_file = File.open(path)
      knowledge_yml = knowledge_file.read
      knowledge_hash = YAML.load(knowledge_yml)
      return knowledge_hash
    end

    def initialize(hash={})
      # hash�̒��g��������Ȃ�A�����ł��
      super(hash) # ����͕K�{

      # ������ {} �Ȃ�V�K�쐬
      # (�p�X���w�肹���ɒP��Knowledge�I�u�W�F�N�g���쐬���邾���A
      # �@�\�Ƃ��Ă͎����͌��)�Ƃ��������ɂ��A�p�X�̏����͍s��Ȃ��B
      unless hash=={}
        # === �p�X�̏���
        # * Local:
        #    * �X�[�p�[���[�U�̏ꍇ
               #�K����΃p�X�Ƃ݂Ȃ�(�ǂ��ɂł��t�@�C����u����)
        #    * ��ʃ��[�U�̏ꍇ
        #      "/usr/" + user_name + "/knowledge/" + @path �̌`��
        #      �Ȃ��Ă��Ȃ���Α��΃p�X�Ƃ݂Ȃ�(/knowledge/�ȉ��ɂ����t�@�C����u���Ȃ�)
        #    * ���[�U�w��Ȃ��̏ꍇ
        #      ��ʃ��[�U�ɓ����B
        # * Remote: 
        #      http:// ����͂��܂�̂ŁA�p�X���ȗ�����邱�Ƃ͂Ȃ�
        path = @object.path
        if @user
          unless @user.super_user
            unless /^\/usr\/(.*)\/knowledge\/.*/ =~ path
              path = "/usr/" + user_name + "/knowledge/" + path
            end
          end
        end
        @object.path = path
        @object.owner = @user
      end
    end
  end
end

